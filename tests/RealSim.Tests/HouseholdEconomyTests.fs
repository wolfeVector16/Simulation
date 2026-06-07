namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain

module HouseholdEconomyTests =
    let private firstHousehold world =
        world.Households |> Map.toSeq |> Seq.head

    let private mapHouseholds f world =
        { world with Households = world.Households |> Map.map f }

    let private setFirstHousehold f world =
        let householdId, _ = firstHousehold world
        { world with Households = world.Households |> Map.change householdId (Option.map f) }

    let private weeklyWorld () =
        { TestWorld.create () with Day = 7; MinuteOfDay = 9 * 60 }

    [<Fact>]
    let ``TickHouseholdsDoesNotAccrueBillsEveryTick`` () =
        let world =
            TestWorld.create ()
            |> mapHouseholds (fun _ household -> { household with BillsDue = 123m })

        let ticked = LifeSim.tickHouseholds 60 world.Households

        ticked |> Map.iter (fun _ household -> Assert.Equal(123m, household.BillsDue))

    [<Fact>]
    let ``WeeklyBillingAddsNewBillOncePerWeek`` () =
        let dueEvents = SimulationPipeline.generateWeeklyBillingEvents (weeklyWorld ())
        let offCycleEvents = SimulationPipeline.generateWeeklyBillingEvents { weeklyWorld () with MinuteOfDay = 8 * 60 }

        Assert.Contains(dueEvents, function BillDue _ -> true | _ -> false)
        Assert.DoesNotContain(offCycleEvents, function BillDue _ -> true | _ -> false)

    [<Fact>]
    let ``WeeklyBillingDoesNotDuplicateWithinSameWeek`` () =
        let world = weeklyWorld ()
        let firstEvents = SimulationPipeline.generateWeeklyBillingEvents world
        let afterFirst = SimulationPipeline.applyEvents firstEvents world
        let secondEvents = SimulationPipeline.generateWeeklyBillingEvents afterFirst

        Assert.Contains(firstEvents, function BillDue _ -> true | _ -> false)
        Assert.Empty(secondEvents |> List.filter (function BillDue _ -> true | _ -> false))

    [<Fact>]
    let ``LateFeeAppliesOnlyToPreviousUnpaidBills`` () =
        let world = weeklyWorld () |> setFirstHousehold (fun household -> { household with BillsDue = 1000m; Funds = 0m })
        let householdId, before = firstHousehold world
        let weeklyBill = HouseholdEconomy.calculateWeeklyHouseholdBill world before
        let events = SimulationPipeline.generateWeeklyBillingEvents world
        let after = (SimulationPipeline.applyEvents events world).Households[householdId]

        Assert.Contains(events, function BillMissed(_, id, LegacyBill, amount) -> id = householdId && amount = 1000m | _ -> false)
        Assert.Equal(1000m + weeklyBill + 40m, after.BillsDue)

    [<Fact>]
    let ``NoLateFeeWhenNoPreviousBalance`` () =
        let world = weeklyWorld () |> setFirstHousehold (fun household -> { household with BillsDue = 0m })
        let events = SimulationPipeline.generateWeeklyBillingEvents world

        Assert.Contains(events, function BillDue _ -> true | _ -> false)
        Assert.DoesNotContain(events, function BillMissed _ -> true | _ -> false)

    [<Fact>]
    let ``MissedBillLateFeeIsCapped`` () =
        Assert.Equal(40m, HouseholdEconomy.lateFeeForPreviousBalance 10_000m)
        Assert.Equal(40m, HouseholdEconomy.lateFeeForPreviousBalance Decimal.MaxValue)

    [<Fact>]
    let ``BillMissedReducerAddsOnlyLateFeeNotPrincipal`` () =
        let world = TestWorld.create () |> setFirstHousehold (fun household -> { household with BillsDue = 1000m })
        let householdId, _ = firstHousehold world
        let after = SimulationPipeline.applyEventAndRemember (BillMissed(TestIds.eventId 301, householdId, LegacyBill, 1000m)) world

        Assert.Equal(1040m, after.Households[householdId].BillsDue)

    [<Fact>]
    let ``HouseholdIntentDoesNotRunEveryHour`` () =
        let world =
            { TestWorld.create () with Day = 1; MinuteOfDay = 10 * 60 }
            |> mapHouseholds (fun _ household -> { household with BillsDue = 100m; Funds = 0m })

        let next = SimulationPipeline.tick world

        Assert.DoesNotContain(next.Meta.Decisions, fun decision ->
            match decision.ChosenAction with
            | PayBillAction _
            | DelayBillAction _ -> true
            | _ -> false)

    [<Fact>]
    let ``HouseholdIntentDoesNotRunDaily`` () =
        let world =
            { TestWorld.create () with Day = 8; MinuteOfDay = 9 * 60 }
            |> mapHouseholds (fun _ household -> { household with BillsDue = 100m; Funds = 0m })

        let next = SimulationPipeline.tick world

        Assert.DoesNotContain(next.Meta.Decisions, fun decision ->
            match decision.ChosenAction with
            | PayBillAction _
            | DelayBillAction _ -> true
            | _ -> false)

    [<Fact>]
    let ``HouseholdIntentUsesPaymentDecisionNotLateFeeCompounding`` () =
        let world = weeklyWorld () |> setFirstHousehold (fun household -> { household with BillsDue = 1000m; Funds = 0m })
        let householdId, before = firstHousehold world
        let weeklyBill = HouseholdEconomy.calculateWeeklyHouseholdBill world before
        let first = SimulationPipeline.tick world
        let afterFirst = first.Households[householdId].BillsDue
        let second = SimulationPipeline.tick first

        Assert.Equal(1000m + weeklyBill + 40m, afterFirst)
        Assert.Equal(afterFirst, second.Households[householdId].BillsDue)
        Assert.DoesNotContain(first.Meta.EventLog, function
            | BillMissed(_, id, LegacyBill, amount) when id = householdId -> amount <> 1000m
            | _ -> false)

    [<Fact>]
    let ``BillDueAmountIsBounded`` () =
        let world = TestWorld.create () |> setFirstHousehold (fun household -> { household with BillsDue = 0m })
        let householdId, _ = firstHousehold world
        let after = SimulationPipeline.applyEventAndRemember (BillDue(TestIds.eventId 302, householdId, LegacyBill, Decimal.MaxValue)) world

        Assert.True(after.Households[householdId].BillsDue <= HouseholdEconomy.maxHouseholdBillsDue)
        Assert.Equal(HouseholdEconomy.maxWeeklyHouseholdBill, after.Households[householdId].BillsDue)

    [<Fact>]
    let ``BillPaidCannotMakeBillsDueNegative`` () =
        let world = TestWorld.create () |> setFirstHousehold (fun household -> { household with BillsDue = 100m; Funds = 10_000m })
        let householdId, _ = firstHousehold world
        let after =
            SimulationPipeline.applyEventAndRemember
                (BillPaid(TestIds.eventId 303, householdId, LegacyBill, 1_000m, ExternalPrivateSector))
                world

        Assert.Equal(0m, after.Households[householdId].BillsDue)

    [<Fact>]
    let ``ReducerCapsAlreadyCorruptedBillsDue`` () =
        let corrupted = 77377165827132381138184837903m
        let world = TestWorld.create () |> setFirstHousehold (fun household -> { household with BillsDue = corrupted })
        let householdId, _ = firstHousehold world
        let normalized = HouseholdEconomy.normalizeWorldHouseholdEconomy world
        let after = SimulationPipeline.applyEventAndRemember (BillDue(TestIds.eventId 304, householdId, LegacyBill, 1m)) world

        Assert.True(normalized.Households[householdId].BillsDue <= HouseholdEconomy.maxHouseholdBillsDue)
        Assert.True(after.Households[householdId].BillsDue <= HouseholdEconomy.maxHouseholdBillsDue)

    [<Fact>]
    let ``RentIncreaseDoesNotDoubleCountForever`` () =
        let world = TestWorld.create ()
        let householdId, before = firstHousehold world
        let oldExpenses = before.MonthlyExpenses
        let first = SimulationPipeline.applyEventAndRemember (RentIncreased(TestIds.eventId 305, householdId, before.RentMonthly |> Option.defaultValue 0m, 2_000m)) world
        let second = SimulationPipeline.applyEventAndRemember (RentIncreased(TestIds.eventId 306, householdId, 2_000m, 2_000m)) first

        Assert.Equal(oldExpenses - (before.RentMonthly |> Option.defaultValue 0m) + 2_000m, first.Households[householdId].MonthlyExpenses)
        Assert.Equal(first.Households[householdId].MonthlyExpenses, second.Households[householdId].MonthlyExpenses)

    [<Fact>]
    let ``HouseholdEconomyInvariantFlagsImpossibleBillsDue`` () =
        let corrupted = 77377165827132381138184837903m
        let world = TestWorld.create () |> setFirstHousehold (fun household -> { household with BillsDue = corrupted })

        Assert.ThrowsAny<Exception>(fun () -> Invariants.checkWorld world |> ignore) |> ignore
