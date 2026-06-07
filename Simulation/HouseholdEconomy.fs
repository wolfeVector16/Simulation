namespace Simulation

open Simulation.Domain

module HouseholdEconomy =
    let maxHouseholdBillsDue = 250_000m
    let maxWeeklyHouseholdBill = 25_000m
    let maxHouseholdLateFee = 40m
    let maxReasonableMonthlyExpense = 100_000m
    let maxReasonableWeeklyExpense = 25_000m
    let maxReasonableRent = 50_000m
    let maxReasonableHouseholdFunds = 1_000_000m

    let clampMoney minimum maximum amount =
        amount |> max minimum |> min maximum

    let normalizeBillsDue amount =
        clampMoney 0m maxHouseholdBillsDue amount

    let boundedWeeklyBill amount =
        clampMoney 0m maxWeeklyHouseholdBill amount

    let lateFeeForPreviousBalance previousBalance =
        let previousBalance = normalizeBillsDue previousBalance

        if previousBalance <= 0m then
            0m
        else
            min maxHouseholdLateFee (previousBalance * 0.05m)

    let private householdUnit world householdId =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.tryFind (fun (_, unit) -> Set.contains householdId unit.Occupants)
        |> Option.map snd

    let private institutionOwner unit =
        match unit.Owner with
        | InstitutionOwner institutionId -> Some institutionId
        | _ -> None

    let rec billRecipient _world _householdId billKind =
        match billKind with
        | TaxBill
        | FineBill -> CityRecipient
        | UtilityBill(Some provider) -> InstitutionRecipient provider
        | UtilityBill None -> CityRecipient
        | RentBill(Some landlord) -> LandlordRecipient landlord
        | RentBill None -> ExternalPrivateSector
        | MortgageBill(Some lender) -> InstitutionRecipient lender
        | MortgageBill None -> ExternalFinanceSector
        | ServiceFeeBill(Some provider) -> InstitutionRecipient provider
        | ServiceFeeBill None -> ExternalServiceSector
        | PrivateDebtBill(Some lender) -> InstitutionRecipient lender
        | PrivateDebtBill None -> ExternalFinanceSector
        | LateFeeBill originalKind -> billRecipient _world _householdId originalKind
        | LegacyBill -> ExternalPrivateSector

    let calculateWeeklyHouseholdCharges world (household: Household) =
        let rent =
            household.RentMonthly
            |> Option.defaultValue 0m
            |> clampMoney 0m maxReasonableRent

        let monthlyExpenses =
            household.MonthlyExpenses
            |> clampMoney 0m maxReasonableMonthlyExpense

        let monthlyBase = max monthlyExpenses rent
        let utilities = clampMoney 0m maxReasonableWeeklyExpense (household.LotValue * 0.0002m)
        let unit = householdUnit world household.Id
        let landlord = unit |> Option.bind institutionOwner
        let mortgage = unit |> Option.bind _.MortgageMonthly |> Option.defaultValue 0m
        let propertyTax = household.LotValue * decimal world.City.Budget.Taxes.Residential / 52m

        [ match household.HousingStatus, rent with
          | Rents, rent when rent > 0m ->
              yield
                  { Kind = RentBill landlord
                    Amount = boundedWeeklyBill (rent / 4m) }
          | OwnsHome, _ when mortgage > 0m ->
              yield
                  { Kind = MortgageBill None
                    Amount = boundedWeeklyBill (mortgage / 4m) }
          | _ -> ()
          if utilities > 0m then
              yield
                  { Kind = UtilityBill None
                    Amount = boundedWeeklyBill utilities }
          if household.HousingStatus = OwnsHome && propertyTax > 0m then
              yield
                  { Kind = TaxBill
                    Amount = boundedWeeklyBill propertyTax }
          let nonHousingExpenses = max 0m (monthlyBase - rent)
          if nonHousingExpenses > 0m then
              yield
                  { Kind = ServiceFeeBill None
                    Amount = boundedWeeklyBill (nonHousingExpenses / 4m) } ]
        |> List.filter (fun charge -> charge.Amount > 0m)

    let calculateWeeklyHouseholdBill world household =
        calculateWeeklyHouseholdCharges world household
        |> List.sumBy _.Amount
        |> boundedWeeklyBill

    let applyWeeklyBillingCycle world household =
        let previousUnpaid = normalizeBillsDue household.BillsDue
        let lateFee = lateFeeForPreviousBalance previousUnpaid
        let weeklyBill = calculateWeeklyHouseholdBill world household

        { household with
            BillsDue = normalizeBillsDue (previousUnpaid + lateFee + weeklyBill) }

    let normalizeHouseholdEconomy household =
        { household with
            BillsDue = normalizeBillsDue household.BillsDue
            MonthlyExpenses = clampMoney 0m maxReasonableMonthlyExpense household.MonthlyExpenses
            RentMonthly = household.RentMonthly |> Option.map (fun rent -> clampMoney 0m maxReasonableRent rent)
            Funds = clampMoney 0m maxReasonableHouseholdFunds household.Funds }

    let billingCycle (world: World) =
        world.Day / 7

    let isWeeklyBillingTime (world: World) =
        world.Day % 7 = 0 && world.MinuteOfDay = 9 * 60

    let normalizeWorldHouseholdEconomy world =
        { world with
            Households = world.Households |> Map.map (fun _ household -> normalizeHouseholdEconomy household) }
