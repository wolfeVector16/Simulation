namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module CitySystems =
    let private monthlyFraction minutes =
        float minutes / float (30 * minutesPerDay)

    let private distance a b =
        let dx = a.X - b.X
        let dy = a.Y - b.Y
        sqrt (dx * dx + dy * dy)

    let private clampDemand value =
        value |> max -1.0 |> min 1.0

    let private hasOccupiedBuilding (parcel: Parcel) =
        parcel.Building
        |> Option.exists (fun building -> building.Status = Occupied)

    let private zoneBaseDemand (demand: Demand) (parcel: Parcel) =
        match parcel.Zone with
        | ResidentialZone
        | SingleFamilyResidentialZone
        | MultifamilyResidentialZone -> demand.Residential
        | CommercialZone
        | MixedUseZone
        | NeighborhoodCommercialZone
        | ShoppingCenterZone
        | OfficeZone
        | TransitOrientedZone -> demand.Commercial
        | IndustrialZone
        | AgriculturalZone
        | LightIndustrialZone
        | FlexIndustrialZone
        | WarehouseLogisticsZone
        | MixedUseProductionZone
        | HeavyIndustrialZone
        | HazardousIndustrialZone
        | ExtractiveIndustrialZone
        | WasteManagementZone -> demand.Industrial
        | CivicZone
        | SchoolZone
        | MedicalZone
        | UtilityZone
        | ParkZone
        | ParkOpenSpaceZone
        | SpecialDistrictZone
        | Unzoned -> -0.25

    let private serviceCoverage kind (position: Coordinates) (services: ServiceFacility list) =
        services
        |> List.filter (fun (service: ServiceFacility) -> service.Kind = kind)
        |> List.map (fun (service: ServiceFacility) ->
            let falloff = 1.0 - distance position service.Position / service.CoverageRadius
            clamp01 falloff * service.Effectiveness)
        |> function
            | [] -> 0.0
            | scores -> scores |> List.max |> clamp01

    let private nearbyPollution (position: Coordinates) (parcels: Map<ParcelId, Parcel>) =
        parcels
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.averageBy (fun parcel ->
            let proximity = 1.0 - min 1.0 (distance position parcel.Position / 12.0)
            parcel.Pollution * proximity)

    let private utilityLoad kind (city: CityState) =
        let required =
            city.Parcels
            |> Map.toSeq
            |> Seq.sumBy (fun (_, parcel) ->
                match parcel.Building with
                | Some building when building.Status = Occupied ->
                    let densityMultiplier =
                        match parcel.Density with
                        | LowDensity -> 1.0
                        | MediumDensity -> 1.8
                        | HighDensity -> 3.2

                    match kind, building.Use with
                    | PowerUtility, _ -> float building.Capacity * 0.35 * densityMultiplier
                    | WaterUtility, Housing -> float building.Capacity * 0.20 * densityMultiplier
                    | WaterUtility, Commerce -> float building.Capacity * 0.12 * densityMultiplier
                    | WaterUtility, Industry -> float building.Capacity * 0.28 * densityMultiplier
                    | SewageUtility, _ -> float building.Capacity * 0.16 * densityMultiplier
                    | GarbageUtility, Housing -> float building.Capacity * 0.08 * densityMultiplier
                    | GarbageUtility, Commerce -> float building.Capacity * 0.12 * densityMultiplier
                    | GarbageUtility, Industry -> float building.Capacity * 0.20 * densityMultiplier
                    | _ -> 0.0
                | _ -> 0.0)

        let capacity =
            city.Utilities
            |> List.filter (fun utility -> utility.Kind = kind)
            |> List.sumBy _.Capacity

        required, capacity

    let private updateUtilityUse (city: CityState) =
        let utilityUsages =
            [ PowerUtility; WaterUtility; SewageUtility; GarbageUtility ]
            |> List.map (fun kind -> kind, utilityLoad kind city)
            |> Map.ofList

        let utilities =
            city.Utilities
            |> List.map (fun utility ->
                let required, capacity = Map.find utility.Kind utilityUsages
                let share =
                    if capacity <= 0.0 then
                        0.0
                    else
                        required / capacity

                { utility with Used = min utility.Capacity (utility.Capacity * share) })

        { city with Utilities = utilities }, utilityUsages

    let private utilitySatisfied kind (utilityUsages: Map<UtilityKind, float * float>) =
        let required, capacity = Map.find kind utilityUsages
        capacity >= required && capacity > 0.0

    let private updateParcel utilityUsages (parcels: Map<ParcelId, Parcel>) services (parcel: Parcel) =
        let police = serviceCoverage PoliceService parcel.Position services
        let fire = serviceCoverage FireService parcel.Position services
        let health = serviceCoverage HealthService parcel.Position services
        let education = serviceCoverage EducationService parcel.Position services
        let park = serviceCoverage ParkService parcel.Position services
        let transit = serviceCoverage TransitService parcel.Position services
        let localPollution = nearbyPollution parcel.Position parcels

        let serviceLift = (police + fire + health + education + park + transit) / 6.0
        let utilityPenalty =
            [ if not (utilitySatisfied PowerUtility utilityUsages) then 0.20
              if not (utilitySatisfied WaterUtility utilityUsages) then 0.12
              if not (utilitySatisfied SewageUtility utilityUsages) then 0.10
              if not (utilitySatisfied GarbageUtility utilityUsages) then 0.08 ]
            |> List.sum

        let zonePollution =
            match parcel.Zone with
            | IndustrialZone -> 0.20
            | AgriculturalZone -> 0.08
            | CommercialZone -> 0.04
            | _ -> 0.0

        let pollution = clamp01 (parcel.Pollution * 0.92 + zonePollution + localPollution * 0.10)
        let crime = clamp01 (parcel.Crime * 0.90 + (1.0 - police) * 0.12)
        let fireRisk = clamp01 (parcel.FireRisk * 0.90 + (1.0 - fire) * 0.10 + pollution * 0.05)

        let desirability =
            clamp01 (
                0.35
                + serviceLift * 0.35
                + park * 0.20
                + parcel.LandValue * 0.12
                - pollution * 0.28
                - crime * 0.20
                - fireRisk * 0.12
                - utilityPenalty)

        let landValue =
            clamp01 (
                parcel.LandValue * 0.85
                + desirability * 0.16
                + education * 0.06
                + transit * 0.04
                - pollution * 0.08)

        { parcel with
            Powered = utilitySatisfied PowerUtility utilityUsages
            Watered = utilitySatisfied WaterUtility utilityUsages
            Pollution = pollution
            Crime = crime
            FireRisk = fireRisk
            Desirability = desirability
            LandValue = landValue }

    let private maybeDevelopParcel (demand: Demand) (parcel: Parcel) =
        match parcel.Building, parcel.Zone with
        | None, ResidentialZone when demand.Residential > 0.20 && parcel.Desirability > 0.35 && parcel.RoadConnected && parcel.Powered ->
            let capacity =
                match parcel.Density with
                | LowDensity -> 6
                | MediumDensity -> 28
                | HighDensity -> 140

            { parcel with
                Building =
                    Some
                        { Name = "New homes"
                          Use = Housing
                          Wealth = if parcel.LandValue > 0.70 then HighWealth elif parcel.LandValue > 0.45 then MiddleWealth else LowWealth
                          Capacity = capacity
                          Occupants = max 1 (int (float capacity * min 1.0 demand.Residential))
                          Jobs = 0
                          Status = Occupied } }
        | None, CommercialZone when demand.Commercial > 0.20 && parcel.Desirability > 0.35 && parcel.RoadConnected && parcel.Powered ->
            let jobs = if parcel.Density = HighDensity then 90 elif parcel.Density = MediumDensity then 30 else 8

            { parcel with
                Building =
                    Some
                        { Name = "New storefront"
                          Use = Commerce
                          Wealth = MiddleWealth
                          Capacity = jobs
                          Occupants = 0
                          Jobs = jobs
                          Status = Occupied } }
        | None, IndustrialZone when demand.Industrial > 0.15 && parcel.RoadConnected && parcel.Powered ->
            let jobs = if parcel.Density = HighDensity then 120 elif parcel.Density = MediumDensity then 45 else 18

            { parcel with
                Building =
                    Some
                        { Name = "New workshop"
                          Use = Industry
                          Wealth = LowWealth
                          Capacity = jobs
                          Occupants = 0
                          Jobs = jobs
                          Status = Occupied } }
        | Some building, _ when (parcel.Desirability < 0.18 || not parcel.Powered) && building.Status = Occupied ->
            { parcel with Building = Some { building with Status = Vacant } }
        | Some building, _ when (parcel.Desirability < 0.18 || not parcel.Powered) && building.Status = Vacant ->
            { parcel with Building = Some { building with Status = Abandoned } }
        | Some building, _ when building.Status = Abandoned && parcel.Desirability > 0.45 && parcel.Powered ->
            { parcel with Building = Some { building with Status = Occupied } }
        | _ -> parcel

    let private calculateIndicators (city: CityState) =
        let parcels = city.Parcels |> Map.toSeq |> Seq.map snd |> Seq.toList
        let population = city.Indicators.Population
        let jobs = city.Indicators.Jobs
        let service kind =
            parcels
            |> List.averageBy (fun parcel -> serviceCoverage kind parcel.Position city.Services)

        let averageLandValue = parcels |> List.averageBy _.LandValue
        let averageDesirability = parcels |> List.averageBy _.Desirability
        let pollution = parcels |> List.averageBy _.Pollution
        let crime = parcels |> List.averageBy _.Crime
        let fireRisk = parcels |> List.averageBy _.FireRisk

        let traffic =
            let totalCapacity = city.Services |> List.filter (fun s -> s.Kind = TransitService) |> List.sumBy _.Capacity
            let roadPressure = if jobs = 0 then 0.0 else min 1.0 (float population / float jobs)
            clamp01 (roadPressure - totalCapacity / 1000.0)

        { Population = population
          Jobs = jobs
          Unemployment = city.Indicators.Unemployment
          AverageLandValue = averageLandValue
          AverageDesirability = averageDesirability
          Pollution = pollution
          Crime = crime
          FireRisk = fireRisk
          Education = service EducationService
          Health = service HealthService
          Traffic = traffic }

    let private calculateDemand (indicators: CityIndicators) (budget: Budget) =
        let taxPenalty rate = (rate - 0.09) * 2.5
        let treasuryPenalty = if budget.Treasury < 0m then 0.20 else 0.0

        { Residential =
            clampDemand (
                0.45
                + indicators.AverageDesirability * 0.45
                - indicators.Pollution * 0.25
                - indicators.Crime * 0.20
                - indicators.Unemployment * 0.35
                - taxPenalty budget.Taxes.Residential
                - treasuryPenalty)
          Commercial =
            clampDemand (
                0.25
                + float indicators.Population / 120.0
                + indicators.AverageLandValue * 0.25
                - indicators.Traffic * 0.25
                - taxPenalty budget.Taxes.Commercial
                - treasuryPenalty)
          Industrial =
            clampDemand (
                0.35
                + max 0.0 (1.0 - indicators.Unemployment) * 0.25
                - indicators.Pollution * 0.10
                - taxPenalty budget.Taxes.Industrial
                - if budget.Taxes.Industrial > 0.12 then 0.20 else 0.0) }

    let private calculateBudget minutes (city: CityState) (_indicators: CityIndicators) =
        let monthlyTaxIncome =
            city.Parcels
            |> Map.toSeq
            |> Seq.sumBy (fun (_, parcel) ->
                match parcel.Building with
                | Some building when building.Status = Occupied ->
                    let baseValue = decimal (parcel.LandValue * float building.Capacity * 1000.0)

                    match building.Use with
                    | Housing -> baseValue * decimal city.Budget.Taxes.Residential
                    | Commerce -> baseValue * decimal city.Budget.Taxes.Commercial
                    | Industry -> baseValue * decimal city.Budget.Taxes.Industrial
                    | PublicService
                    | Recreation -> 0m
                | _ -> 0m)

        let serviceCosts = city.Services |> List.sumBy _.MonthlyCost
        let utilityCosts = city.Utilities |> List.sumBy _.MonthlyCost
        let policyCosts =
            [ if city.Policies.RecyclingProgram then 180m
              if city.Policies.SmokeDetectors then 80m
              if city.Policies.CarpoolIncentives then 120m
              if city.Policies.CleanAirAct then 220m
              if city.Policies.EducationCampaign then 150m ]
            |> List.sum

        let interest = city.Budget.Debt * decimal city.Budget.InterestRate / 12m
        let monthlyExpenses = serviceCosts + utilityCosts + policyCosts + interest
        let flow = (monthlyTaxIncome - monthlyExpenses) * decimal (monthlyFraction minutes)

        { city.Budget with
            Treasury = city.Budget.Treasury + flow
            MonthlyIncome = monthlyTaxIncome
            MonthlyExpenses = monthlyExpenses }

    let private advisor severity department message =
        { Severity = severity
          Department = department
          Message = message }

    let private buildAdvisors utilityUsages (city: CityState) =
        let indicators = city.Indicators
        let checkUtility kind label =
            [ let required, capacity = Map.find kind utilityUsages

              if capacity <= 0.0 || required > capacity then
                  yield advisor Critical "Utilities" (sprintf "%s demand is %.0f but capacity is %.0f." label required capacity) ]

        [ yield! checkUtility PowerUtility "Power"
          yield! checkUtility WaterUtility "Water"
          yield! checkUtility SewageUtility "Sewage"
          yield! checkUtility GarbageUtility "Garbage"

          if indicators.Crime > 0.35 then
              yield advisor Warning "Police" "Crime is rising in poorly covered neighborhoods."

          if indicators.FireRisk > 0.40 then
              yield advisor Warning "Fire" "Fire risk is elevated. Add coverage near dense or industrial areas."

          if indicators.Pollution > 0.35 then
              yield advisor Warning "Environment" "Pollution is suppressing desirability and land value."

          if indicators.Education < 0.35 then
              yield advisor Warning "Education" "Education coverage is weak, limiting long-term workforce quality."

          if indicators.Unemployment > 0.25 then
              yield advisor Warning "Economy" "Residential population is outpacing available jobs."

          if city.Budget.Treasury < 0m then
              yield advisor Critical "Budget" "The city treasury is negative."
          elif city.Budget.MonthlyExpenses > city.Budget.MonthlyIncome then
              yield advisor Warning "Budget" "Monthly expenses exceed income."

          if city.Demand.Residential > 0.60 then
              yield advisor Info "Planning" "Residential demand is strong. More serviced housing can grow."

          if city.Demand.Commercial > 0.60 then
              yield advisor Info "Planning" "Commercial demand is strong around desirable, accessible parcels." ]

    let tick minutes city =
        let city, utilityUsages = updateUtilityUse city

        let parcels =
            city.Parcels
            |> Map.map (fun _ parcel -> updateParcel utilityUsages city.Parcels city.Services parcel)
            |> Map.map (fun _ parcel -> maybeDevelopParcel city.Demand parcel)

        let city = { city with Parcels = parcels }
        let indicators = calculateIndicators city
        let demand = calculateDemand indicators city.Budget
        let budget = calculateBudget minutes city indicators
        let city = { city with Indicators = indicators; Demand = demand; Budget = budget }
        { city with Advisors = buildAdvisors utilityUsages city }
