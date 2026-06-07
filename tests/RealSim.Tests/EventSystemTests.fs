namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain

module EventSystemTests =
    [<Fact>]
    let ``RentIncreasedUpdatesHouseholdPressure`` () =
        let world = TestWorld.create ()
        let householdId, before = world.Households |> Map.toSeq |> Seq.head
        let event = RentIncreased(TestIds.eventId 1, householdId, before.RentMonthly |> Option.defaultValue 1000m, 1800m)

        let next = SimulationPipeline.applyEventAndRemember event world
        let after = next.Households[householdId]

        Assert.Equal(Some 1800m, after.RentMonthly)
        Assert.True(after.MonthlyExpenses > before.MonthlyExpenses)
        Assert.True(after.Stability < before.Stability)
        Assert.NotEmpty(next.Memories)
        Invariants.checkWorld next |> ignore

    [<Fact>]
    let ``InsufficientHouseholdFundsMissesBillWithReasons`` () =
        let initial = TestWorld.create ()
        let world =
            { initial with
                MinuteOfDay = 7 * 60
                Households =
                    initial.Households
                    |> Map.map (fun _ household ->
                        { household with
                            Funds = 10m
                            BillsDue = 100m
                            Stability = 0.40 }) }

        let next = SimulationPipeline.tick world

        Assert.Contains(next.Meta.EventLog, function BillMissed _ -> true | _ -> false)
        Assert.Contains(next.Meta.Decisions, fun decision ->
            decision.ChosenAction = DelayBillAction 100m
            && List.contains FinancialPressure decision.Reasons
            && List.contains HousingInstability decision.Reasons)

        next.Households
        |> Map.iter (fun _ household -> Assert.True(household.Stability < 0.40 || household.BillsDue > 100m))

        Invariants.checkWorld next |> ignore

    [<Fact>]
    let ``ImportantEventsCreateMemoriesAndDecay`` () =
        let world = TestWorld.create ()
        let householdId, household = world.Households |> Map.toSeq |> Seq.head
        let event = RentIncreased(TestIds.eventId 2, householdId, household.RentMonthly |> Option.defaultValue 1000m, 1900m)
        let withMemory = SimulationPipeline.applyEventAndRemember event world
        let memoryId, memory = withMemory.Memories |> Map.toSeq |> Seq.head

        let decayed = SimulationPipeline.tick withMemory
        let decayedMemory = decayed.Memories[memoryId]

        Assert.Equal(Important, memory.Salience)
        Assert.True(decayedMemory.EmotionalWeight < memory.EmotionalWeight)
        Invariants.checkWorld decayed |> ignore

    [<Fact>]
    let ``TripDelayedAffectsStressAndSchedule`` () =
        let world = TestWorld.create ()
        let simId, sim = world.Sims |> Map.toSeq |> Seq.find (fun (_, sim) -> sim.Job.IsSome)
        let beforeHousehold = world.Households[sim.Household]
        let event = TransportEventOccurred(TestIds.eventId 3, ArrivedLate(simId, WorkTrip, 35))

        let next = SimulationPipeline.applyEventAndRemember event world
        let after = next.Sims[simId]
        let afterHousehold = next.Households[sim.Household]

        Assert.True(after.Happiness < sim.Happiness)
        Assert.True(afterHousehold.Stability < beforeHousehold.Stability)
        Assert.NotEmpty(after.Memories)
        Invariants.checkWorld next |> ignore

    [<Fact>]
    let ``ApplyingEventsPreservesWorldInvariants`` () =
        let world = TestWorld.create ()
        let simId, _ = world.Sims |> Map.toSeq |> Seq.head
        let householdId, household = world.Households |> Map.toSeq |> Seq.head

        let events =
            [ RentIncreased(TestIds.eventId 4, householdId, household.RentMonthly |> Option.defaultValue 1000m, 1600m)
              TransportEventOccurred(TestIds.eventId 5, ArrivedLate(simId, SchoolTrip, 12)) ]

        let next = SimulationPipeline.applyEvents events world

        Assert.Equal(2, next.Memories.Count)
        Invariants.checkWorld next |> ignore
