namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain

module HouseholdBillRoutingTests =
    let private firstHousehold (world: World) =
        world.Households |> Map.toSeq |> Seq.head

    let private firstInstitution (world: World) =
        world.Institutions |> Map.toSeq |> Seq.head

    let private prepareHousehold amount (world: World) =
        let householdId, _ = firstHousehold world

        { world with
            Households =
                world.Households
                |> Map.change householdId (Option.map (fun household ->
                    { household with
                        Funds = amount * 2m
                        BillsDue = amount })) },
        householdId

    let private pay kind recipient amount world householdId =
        SimulationPipeline.applyEventAndRemember
            (BillPaid(TestIds.eventId (700 + int amount), householdId, kind, amount, recipient))
            world

    [<Fact>]
    let ``TaxBillPaymentIncreasesCityBudget`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 100m
        let beforeTreasury = world.City.Budget.Treasury
        let beforeIncome = world.City.Budget.MonthlyIncome
        let beforeFunds = world.Households[householdId].Funds
        let next = pay TaxBill CityRecipient 100m world householdId

        Assert.Equal(beforeFunds - 100m, next.Households[householdId].Funds)
        Assert.Equal(beforeTreasury + 100m, next.City.Budget.Treasury)
        Assert.Equal(beforeIncome + 100m, next.City.Budget.MonthlyIncome)

    [<Fact>]
    let ``UtilityBillPaymentIncreasesCityOrProviderRevenue`` () =
        let cityWorld, cityHouseholdId = TestWorld.create () |> prepareHousehold 80m
        let cityNext = pay (UtilityBill None) CityRecipient 80m cityWorld cityHouseholdId

        let providerId, provider = firstInstitution cityWorld
        let providerWorld, providerHouseholdId = TestWorld.create () |> prepareHousehold 80m
        let providerNext = pay (UtilityBill(Some providerId)) (InstitutionRecipient providerId) 80m providerWorld providerHouseholdId

        Assert.Equal(cityWorld.City.Budget.Treasury + 80m, cityNext.City.Budget.Treasury)
        Assert.Equal(provider.Funding + 80m, providerNext.Institutions[providerId].Funding)
        Assert.Equal(providerWorld.City.Budget.Treasury, providerNext.City.Budget.Treasury)

    [<Fact>]
    let ``RentBillPaymentGoesToLandlord`` () =
        let landlordId, landlord =
            TestWorld.create().Institutions
            |> Map.toSeq
            |> Seq.find (fun (_, institution) -> institution.Kind = LandlordInstitution)

        let world, householdId = TestWorld.create () |> prepareHousehold 300m
        let beforeTreasury = world.City.Budget.Treasury
        let next = pay (RentBill(Some landlordId)) (LandlordRecipient landlordId) 300m world householdId

        Assert.Equal(landlord.Funding + 300m, next.Institutions[landlordId].Funding)
        Assert.Equal(beforeTreasury, next.City.Budget.Treasury)

    [<Fact>]
    let ``RentBillWithoutModeledLandlordGoesToExternalPrivateSector`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 250m
        let next = pay (RentBill None) ExternalPrivateSector 250m world householdId

        Assert.Equal(world.City.Budget.Treasury, next.City.Budget.Treasury)
        Assert.Equal(world.ExternalLedger.PrivateSectorOutflow + 250m, next.ExternalLedger.PrivateSectorOutflow)

    [<Fact>]
    let ``MortgagePaymentGoesToFinanceSector`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 400m
        let next = pay (MortgageBill None) ExternalFinanceSector 400m world householdId

        Assert.Equal(world.City.Budget.Treasury, next.City.Budget.Treasury)
        Assert.Equal(world.ExternalLedger.FinanceOutflow + 400m, next.ExternalLedger.FinanceOutflow)

    [<Fact>]
    let ``FinePaymentIncreasesCityBudget`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 60m
        let next = pay FineBill CityRecipient 60m world householdId

        Assert.Equal(world.City.Budget.Treasury + 60m, next.City.Budget.Treasury)

    [<Fact>]
    let ``PrivateDebtPaymentDoesNotIncreaseCityBudget`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 125m
        let next = pay (PrivateDebtBill None) ExternalFinanceSector 125m world householdId

        Assert.Equal(world.City.Budget.Treasury, next.City.Budget.Treasury)
        Assert.Equal(world.ExternalLedger.FinanceOutflow + 125m, next.ExternalLedger.FinanceOutflow)

    [<Fact>]
    let ``WeeklyBillingGeneratesTypedCharges`` () =
        let world = { TestWorld.create () with Day = 7; MinuteOfDay = 9 * 60 }
        let householdId, household = firstHousehold world
        let renterCharges = HouseholdEconomy.calculateWeeklyHouseholdCharges world household
        let ownerCharges =
            HouseholdEconomy.calculateWeeklyHouseholdCharges
                world
                { household with
                    HousingStatus = OwnsHome
                    RentMonthly = None }

        Assert.Contains(renterCharges, fun charge -> match charge.Kind with RentBill _ -> true | _ -> false)
        Assert.Contains(renterCharges, fun charge -> match charge.Kind with UtilityBill _ -> true | _ -> false)
        Assert.Contains(ownerCharges, fun charge -> charge.Kind = TaxBill)
        Assert.True(renterCharges.Length > 1)
        Assert.All(SimulationPipeline.generateWeeklyBillingEvents world, fun event ->
            match event with
            | BillDue(_, id, kind, amount) when id = householdId -> Assert.True(amount > 0m && kind <> LegacyBill)
            | _ -> ())

    [<Fact>]
    let ``LateFeeRoutesToOriginalBillRecipient`` () =
        let landlordId, _ =
            TestWorld.create().Institutions
            |> Map.toSeq
            |> Seq.find (fun (_, institution) -> institution.Kind = LandlordInstitution)

        let rentWorld, rentHouseholdId = TestWorld.create () |> prepareHousehold 40m
        let rentNext = pay (LateFeeBill(RentBill(Some landlordId))) (LandlordRecipient landlordId) 40m rentWorld rentHouseholdId

        let taxWorld, taxHouseholdId = TestWorld.create () |> prepareHousehold 40m
        let taxNext = pay (LateFeeBill TaxBill) CityRecipient 40m taxWorld taxHouseholdId

        Assert.Equal(rentWorld.Institutions[landlordId].Funding + 40m, rentNext.Institutions[landlordId].Funding)
        Assert.Equal(taxWorld.City.Budget.Treasury + 40m, taxNext.City.Budget.Treasury)

    [<Fact>]
    let ``GenericLegacyBillDoesNotInflateCityBudget`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 90m
        let next = pay LegacyBill ExternalPrivateSector 90m world householdId

        Assert.Equal(world.City.Budget.Treasury, next.City.Budget.Treasury)
        Assert.Equal(world.ExternalLedger.PrivateSectorOutflow + 90m, next.ExternalLedger.PrivateSectorOutflow)

    [<Fact>]
    let ``BillPaymentCannotCreateMoney`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 200m
        let beforeFunds = world.Households[householdId].Funds
        let next = pay (PrivateDebtBill None) ExternalFinanceSector 200m world householdId
        let householdDecrease = beforeFunds - next.Households[householdId].Funds
        let externalIncrease = next.ExternalLedger.FinanceOutflow - world.ExternalLedger.FinanceOutflow

        Assert.Equal(householdDecrease, externalIncrease)

    [<Fact>]
    let ``BillPaymentCannotMakeBillsDueNegative`` () =
        let world, householdId = TestWorld.create () |> prepareHousehold 100m
        let next = pay TaxBill CityRecipient 250m world householdId

        Assert.Equal(0m, next.Households[householdId].BillsDue)
        Assert.Equal(world.City.Budget.Treasury + 100m, next.City.Budget.Treasury)

    [<Fact>]
    let ``CityBudgetDoesNotReceiveMortgageOrPrivateRent`` () =
        let mortgageWorld, mortgageHouseholdId = TestWorld.create () |> prepareHousehold 100m
        let mortgageNext = pay (MortgageBill None) ExternalFinanceSector 100m mortgageWorld mortgageHouseholdId

        let rentWorld, rentHouseholdId = TestWorld.create () |> prepareHousehold 100m
        let rentNext = pay (RentBill None) ExternalPrivateSector 100m rentWorld rentHouseholdId

        Assert.Equal(mortgageWorld.City.Budget.Treasury, mortgageNext.City.Budget.Treasury)
        Assert.Equal(rentWorld.City.Budget.Treasury, rentNext.City.Budget.Treasury)
