namespace Simulation

open System
open System.Text
open Simulation.Domain

module CollapseDiagnostics =
    type CollapseTracePoint =
        { Tick: int
          Day: int
          MinuteOfDay: int
          CityJobsMetric: int
          DerivedJobCount: int
          GeneratedJobCount: int
          FilledJobCount: int
          OpenJobCount: int
          EmployerCount: int
          BusinessCount: int
          InstitutionEmployerCapacityCount: int
          CityPopulationMetric: int
          SimCount: int
          HouseholdCount: int
          OccupiedHousingUnits: int
          VacantHousingUnits: int
          Unemployment: float
          AverageHouseholdFunds: decimal
          MinimumHouseholdFunds: decimal
          AverageHouseholdStability: float
          MinimumHouseholdStability: float
          RentPressure: float
          CommercialDemand: float
          IndustrialDemand: float
          ResidentialDemand: float
          Events: DomainEvent list
          Warnings: string list }

    type EventCauseSummary =
        { Tick: int
          JobDelta: int
          PopulationDelta: int
          JobCreatingEvents: DomainEvent list
          JobDestroyingEvents: DomainEvent list
          PopulationCreatingEvents: DomainEvent list
          PopulationDestroyingEvents: DomainEvent list
          NeutralEvents: DomainEvent list
          Warnings: string list }

    type CollapseTrace =
        { Points: CollapseTracePoint list
          CauseSummaries: EventCauseSummary list
          Report: string }

    let derivePopulationFromWorld = CityMetrics.derivePopulation

    let deriveJobsFromWorld = CityMetrics.deriveJobs

    let deriveFilledJobsFromWorld = CityMetrics.deriveFilledJobs

    let deriveOpenJobsFromWorld = CityMetrics.deriveOpenJobs

    let private occupiedHousingUnits world =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.filter (fun (_, unit) -> not unit.Vacancy || not unit.Occupants.IsEmpty)
        |> Seq.length

    let private vacantHousingUnits world =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.filter (fun (_, unit) -> unit.Vacancy && unit.Occupants.IsEmpty)
        |> Seq.length

    let private averageDecimalOrZero values =
        let values = values |> Seq.toList
        if values.IsEmpty then 0m else values |> List.average

    let private minDecimalOrZero values =
        let values = values |> Seq.toList
        if values.IsEmpty then 0m else values |> List.min

    let private averageFloatOrZero values =
        let values = values |> Seq.toList
        if values.IsEmpty then 0.0 else values |> List.average

    let private minFloatOrZero values =
        let values = values |> Seq.toList
        if values.IsEmpty then 0.0 else values |> List.min

    let private eventId event =
        match event with
        | PersonMoved(id, _, _, _)
        | JobStarted(id, _, _)
        | JobLost(id, _, _)
        | RentIncreased(id, _, _, _)
        | BillDue(id, _, _, _)
        | BillPaid(id, _, _, _, _)
        | BillMissed(id, _, _, _)
        | EvictionFiled(id, _, _)
        | EvictionCompleted(id, _, _)
        | HouseholdCreated(id, _, _)
        | HouseholdMovedIn(id, _, _)
        | IllnessOccurred(id, _)
        | RelationshipChanged(id, _, _)
        | ConflictOccurred(id, _, _, _)
        | ChildMissedSchool(id, _, _)
        | SchoolDayCompleted(id, _, _)
        | CrimeOccurred(id, _, _)
        | PoliceInteractionOccurred(id, _, _)
        | HospitalVisitOccurred(id, _, _)
        | BusinessOpened(id, _)
        | BusinessClosed(id, _)
        | PolicyPassed(id, _)
        | ServiceCapacityChanged(id, _, _, _)
        | NeighborhoodReputationChanged(id, _, _)
        | HouseholdBudgetChanged(id, _, _)
        | TransportEventOccurred(id, _)
        | ActorMoved(id, _, _, _)
        | ActorEnteredVehicle(id, _, _)
        | ActorExitedVehicle(id, _, _, _)
        | ActorGainedVehicleControl(id, _, _)
        | ActorLostVehicleControl(id, _, _)
        | VehicleMoved(id, _, _, _)
        | VehicleCollisionOccurred(id, _, _)
        | ActorEnteredBuilding(id, _, _)
        | ActorExitedBuilding(id, _, _, _)
        | UnauthorizedEntryAttempted(id, _, _)
        | UnauthorizedEntrySucceeded(id, _, _)
        | UnauthorizedEntryFailed(id, _, _)
        | UnauthorizedVehicleAccessAttempted(id, _, _)
        | UnauthorizedVehicleAccessSucceeded(id, _, _)
        | UnauthorizedVehicleAccessFailed(id, _, _)
        | VehicleDamaged(id, _, _)
        | VehicleAlarmTriggered(id, _)
        | ItemPurchased(id, _, _, _, _)
        | ItemTakenWithoutPayment(id, _, _, _)
        | ObjectUsed(id, _, _)
        | PersonInteractionOccurred(id, _, _)
        | ConflictStarted(id, _, _)
        | ConflictEscalated(id, _, _)
        | ConflictResolved(id, _, _)
        | PropertyDamaged(id, _, _, _)
        | TheftReported(id, _, _)
        | TrespassReported(id, _, _)
        | CrimeReported(id, _, _, _)
        | WitnessObservedEvent(id, _, _, _, _)
        | PoliceDispatched(id, _)
        | PoliceArrived(id, _)
        | EmergencyServiceCalled(id, _, _)
        | EmergencyServiceArrived(id, _)
        | ActorDetained(id, _, _)
        | ActorFined(id, _, _)
        | ActorReleased(id, _)
        | ActorInjured(id, _)
        | BusinessInterrupted(id, _, _)
        | NeighborhoodSafetyChanged(id, _, _)
        | InstitutionalTrustChanged(id, _, _, _)
        | ReputationChanged(id, _, _)
        | WantedLevelChanged(id, _, _, _)
        | HeatIncreased(id, _, _, _)
        | HeatDecreased(id, _, _, _)
        | RoadBuilt(id, _, _, _, _)
        | RoadModified(id, _, _, _)
        | RoadDestroyed(id, _, _, _)
        | RoadDamaged(id, _, _, _)
        | RoadClosed(id, _, _, _)
        | RoadReopened(id, _, _)
        | LaneConfigurationChanged(id, _, _, _)
        | IntersectionModified(id, _, _)
        | SignalTimingChanged(id, _, _)
        | TransitRouteCreated(id, _, _)
        | TransitRouteModified(id, _, _)
        | TransitRouteRemoved(id, _, _)
        | UtilityBuilt(id, _, _, _)
        | UtilityDamaged(id, _, _, _)
        | UtilityDisabled(id, _, _)
        | UtilityRestored(id, _, _)
        | ParcelZoned(id, _, _, _, _)
        | ParcelRezoned(id, _, _, _, _)
        | BuildingConstructed(id, _, _, _, _, _)
        | BuildingModified(id, _, _, _)
        | BuildingDamaged(id, _, _, _)
        | BuildingDestroyed(id, _, _, _, _, _)
        | BuildingCondemned(id, _, _, _)
        | BuildingRepaired(id, _, _, _)
        | BuildingAbandoned(id, _, _)
        | HousingUnitsAdded(id, _, _, _)
        | HousingUnitsRemoved(id, _, _, _)
        | HouseholdsDisplaced(id, _, _, _)
        | JobsCreated(id, _, _)
        | JobsLost(id, _, _)
        | InstitutionOpened(id, _, _)
        | InstitutionClosed(id, _, _)
        | InstitutionCapacityChanged(id, _, _, _, _)
        | PolicyRepealed(id, _, _)
        | BudgetChanged(id, _, _, _, _)
        | TaxRateChanged(id, _, _)
        | BondIssued(id, _, _, _, _)
        | DisasterStarted(id, _, _)
        | DisasterEnded(id, _)
        | EmergencyActionTaken(id, _, _, _) -> id

    let private newEvents before after =
        let beforeIds = before.Meta.EventLog |> List.map eventId |> Set.ofList

        after.Meta.EventLog
        |> List.filter (fun event -> not (Set.contains (eventId event) beforeIds))
        |> List.rev

    let private businessCount world =
        world.Map.Places
        |> Map.toSeq
        |> Seq.filter (fun (_, place) -> place.Kind = Commercial || place.Kind = Industrial || place.Kind = Workplace)
        |> Seq.length

    let private employerCount world =
        world.GeneratedJobs
        |> Map.toSeq
        |> Seq.choose (fun (_, job) -> job.Employer)
        |> Seq.distinct
        |> Seq.length

    let private institutionEmployerCapacityCount world =
        world.Institutions
        |> Map.toSeq
        |> Seq.filter (fun (_, institution) -> institution.Kind = EmployerInstitution && institution.Capacity > 0)
        |> Seq.length

    let private warningsFor world =
        [ if world.City.Indicators.Population <> derivePopulationFromWorld world then
              $"Metric divergence: City.Indicators.Population=%d{world.City.Indicators.Population}, derived sims=%d{derivePopulationFromWorld world}."
          if world.City.Indicators.Jobs <> deriveJobsFromWorld world then
              $"Metric divergence: City.Indicators.Jobs=%d{world.City.Indicators.Jobs}, derived generated jobs=%d{deriveJobsFromWorld world}."
          if world.City.Indicators.Unemployment <> 0.0 then
              let employed = deriveFilledJobsFromWorld world
              let adultLaborForce =
                  world.Sims
                  |> Map.toSeq
                  |> Seq.filter (fun (_, sim) -> sim.LifeStage = YoungAdult || sim.LifeStage = Adult || sim.LifeStage = Elder)
                  |> Seq.length

              let actual =
                  if adultLaborForce = 0 then
                      0.0
                  else
                      max 0.0 (float (adultLaborForce - employed) / float adultLaborForce)

              if abs (world.City.Indicators.Unemployment - actual) > 0.001 then
                  $"Metric divergence: City unemployment=%.3f{world.City.Indicators.Unemployment}, derived labor unemployment=%.3f{actual}." ]

    let point events world =
        let funds = world.Households |> Map.toSeq |> Seq.map (fun (_, household) -> household.Funds)
        let stability = world.Households |> Map.toSeq |> Seq.map (fun (_, household) -> household.Stability)

        { Tick = world.Meta.Tick
          Day = world.Day
          MinuteOfDay = world.MinuteOfDay
          CityJobsMetric = world.City.Indicators.Jobs
          DerivedJobCount = deriveJobsFromWorld world
          GeneratedJobCount = world.GeneratedJobs.Count
          FilledJobCount = deriveFilledJobsFromWorld world
          OpenJobCount = deriveOpenJobsFromWorld world
          EmployerCount = employerCount world
          BusinessCount = businessCount world
          InstitutionEmployerCapacityCount = institutionEmployerCapacityCount world
          CityPopulationMetric = world.City.Indicators.Population
          SimCount = world.Sims.Count
          HouseholdCount = world.Households.Count
          OccupiedHousingUnits = occupiedHousingUnits world
          VacantHousingUnits = vacantHousingUnits world
          Unemployment = world.City.Indicators.Unemployment
          AverageHouseholdFunds = averageDecimalOrZero funds
          MinimumHouseholdFunds = minDecimalOrZero funds
          AverageHouseholdStability = averageFloatOrZero stability
          MinimumHouseholdStability = minFloatOrZero stability
          RentPressure = world.Neighborhoods |> Map.toSeq |> Seq.map (fun (_, n) -> n.RentPressure) |> averageFloatOrZero
          CommercialDemand = world.City.Demand.Commercial
          IndustrialDemand = world.City.Demand.Industrial
          ResidentialDemand = world.City.Demand.Residential
          Events = events
          Warnings = warningsFor world }

    let private isJobCreatingEvent =
        function
        | JobsCreated _
        | JobStarted _
        | BusinessOpened _
        | InstitutionOpened _ -> true
        | _ -> false

    let private isJobDestroyingEvent =
        function
        | JobsLost _
        | JobLost _
        | BusinessClosed _
        | InstitutionClosed _
        | BuildingAbandoned(_, _, _)
        | BuildingDestroyed _
        | BuildingCondemned _ -> true
        | _ -> false

    let private isPopulationCreatingEvent =
        function
        | HouseholdCreated _
        | HouseholdMovedIn _
        | HousingUnitsAdded _ -> true
        | _ -> false

    let private isPopulationDestroyingEvent =
        function
        | EvictionCompleted _
        | HouseholdsDisplaced _
        | HousingUnitsRemoved _
        | BuildingDestroyed _
        | BuildingAbandoned(_, _, _)
        | BuildingCondemned _
        | ActorInjured _ -> true
        | _ -> false

    let causeSummary previous current =
        let jobDelta = current.DerivedJobCount - previous.DerivedJobCount
        let populationDelta = current.SimCount - previous.SimCount
        let jobCreating = current.Events |> List.filter isJobCreatingEvent
        let jobDestroying = current.Events |> List.filter isJobDestroyingEvent
        let populationCreating = current.Events |> List.filter isPopulationCreatingEvent
        let populationDestroying = current.Events |> List.filter isPopulationDestroyingEvent
        let classified = jobCreating @ jobDestroying @ populationCreating @ populationDestroying |> Set.ofList
        let neutral = current.Events |> List.filter (fun event -> not (Set.contains event classified))

        { Tick = current.Tick
          JobDelta = jobDelta
          PopulationDelta = populationDelta
          JobCreatingEvents = jobCreating
          JobDestroyingEvents = jobDestroying
          PopulationCreatingEvents = populationCreating
          PopulationDestroyingEvents = populationDestroying
          NeutralEvents = neutral
          Warnings =
            [ if jobDelta < 0 && jobDestroying.IsEmpty then
                  "Unexplained job loss"
              if populationDelta < 0 && populationDestroying.IsEmpty then
                  "Unexplained population loss"
              if current.CityJobsMetric < previous.CityJobsMetric && jobDestroying.IsEmpty then
                  "Unexplained dashboard job loss"
              if current.CityPopulationMetric < previous.CityPopulationMetric && populationDestroying.IsEmpty then
                  "Unexplained dashboard population loss" ] }

    let private formatTime minute =
        let hour = minute / 60
        let minute = minute % 60
        $"%02d{hour}:%02d{minute}"

    let private firstWhere predicate (points: CollapseTracePoint list) =
        points |> List.tryFind predicate

    let private describePoint label (point: CollapseTracePoint option) =
        match point with
        | Some p -> $"%s{label}: tick=%d{p.Tick} day=%d{p.Day} time=%s{formatTime p.MinuteOfDay}"
        | None -> $"%s{label}: not observed"

    let buildReport (points: CollapseTracePoint list) summaries =
        let firstJobsDecrease =
            points
            |> List.pairwise
            |> List.tryPick (fun (previous, current) ->
                if current.CityJobsMetric < previous.CityJobsMetric then Some current else None)

        let firstJobsZero = points |> firstWhere (fun p -> p.CityJobsMetric = 0)

        let firstPopulationDecrease =
            points
            |> List.pairwise
            |> List.tryPick (fun (previous, current) ->
                if current.CityPopulationMetric < previous.CityPopulationMetric then Some current else None)

        let firstPopulationZero = points |> firstWhere (fun p -> p.CityPopulationMetric = 0)

        let firstDerivedJobDrop =
            points
            |> List.pairwise
            |> List.tryPick (fun (previous, current) ->
                if current.DerivedJobCount < previous.DerivedJobCount then Some current else None)

        let firstDerivedPopulationDrop =
            points
            |> List.pairwise
            |> List.tryPick (fun (previous, current) ->
                if current.SimCount < previous.SimCount then Some current else None)

        let firstWarnings =
            points
            |> List.collect (fun p -> p.Warnings |> List.map (fun warning -> p, warning))
            |> List.truncate 8

        let firstCauseWarnings =
            summaries
            |> List.collect (fun s -> s.Warnings |> List.map (fun warning -> s, warning))
            |> List.truncate 8

        let builder = StringBuilder()
        builder.AppendLine("Juniper collapse trace") |> ignore
        builder.AppendLine(describePoint "First dashboard jobs decrease" firstJobsDecrease) |> ignore
        builder.AppendLine(describePoint "First dashboard jobs reach 0" firstJobsZero) |> ignore
        builder.AppendLine(describePoint "First dashboard population decrease" firstPopulationDecrease) |> ignore
        builder.AppendLine(describePoint "First dashboard population reaches 0" firstPopulationZero) |> ignore
        builder.AppendLine(describePoint "First derived jobs decrease" firstDerivedJobDrop) |> ignore
        builder.AppendLine(describePoint "First derived population decrease" firstDerivedPopulationDrop) |> ignore
        builder.AppendLine("Metric divergences:") |> ignore

        if firstWarnings.IsEmpty then
            builder.AppendLine("  none") |> ignore
        else
            for point, warning in firstWarnings do
                builder.AppendLine($"  tick=%d{point.Tick} day=%d{point.Day} %s{formatTime point.MinuteOfDay}: %s{warning}") |> ignore

        builder.AppendLine("Event cause accounting:") |> ignore

        if firstCauseWarnings.IsEmpty then
            builder.AppendLine("  all state deltas explained by classified events") |> ignore
        else
            for summary, warning in firstCauseWarnings do
                builder.AppendLine($"  tick=%d{summary.Tick}: %s{warning}") |> ignore

        builder.AppendLine("Root cause classification: dashboard metrics are derived from canonical world state; parcel/building status changes are reported separately from canonical population and jobs.") |> ignore
        builder.ToString()

    let run tickMinutes ticks initialWorld =
        let rec loop remaining world points summaries =
            if remaining <= 0 then
                let points = List.rev points
                let summaries = List.rev summaries

                { Points = points
                  CauseSummaries = summaries
                  Report = buildReport points summaries }
            else
                let beforePoint = point [] world
                let before = world
                let after = Engine.tick tickMinutes world
                let events = newEvents before after
                let afterPoint = point events after
                let summary = causeSummary beforePoint afterPoint
                loop (remaining - 1) after (afterPoint :: points) (summary :: summaries)

        loop ticks initialWorld [ point [] initialWorld ] []
