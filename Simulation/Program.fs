open Simulation.Domain
open Simulation.Engine
open Simulation.Measures
open RealSim.Scenarios

let formatGood good =
    match good with
    | Groceries -> "groceries"
    | HouseholdGoods -> "household goods"
    | Clothing -> "clothing"
    | Electronics -> "electronics"
    | Entertainment -> "entertainment"
    | RawMaterials -> "raw materials"
    | ManufacturedGoods -> "manufactured goods"
    | LuxuryGoods -> "luxury goods"
    | Toys -> "toys"

let formatIntent intent =
    match intent with
    | NeedPurchase -> "need"
    | WantPurchase -> "want"

let formatSkill skill =
    match skill with
    | Cooking -> "cooking"
    | Charisma -> "charisma"
    | Logic -> "logic"
    | Fitness -> "fitness"
    | Painting -> "painting"
    | Writing -> "writing"
    | Music -> "music"
    | Handiness -> "handiness"
    | Gardening -> "gardening"
    | Programming -> "programming"
    | Creativity -> "creativity"

let formatInteraction interaction =
    match interaction with
    | SleepInBed -> "sleep in bed"
    | CookMeal -> "cook meal"
    | GrabSnack -> "grab snack"
    | ShowerSelf -> "shower"
    | UseToilet -> "use toilet"
    | WatchTv -> "watch TV"
    | PlayGames -> "play games"
    | PracticeSkill skill -> $"practice {formatSkill skill}"
    | CleanObject -> "clean object"
    | RepairObject -> "repair object"
    | PlayWithToys -> "play with toys"
    | ReadBook -> "read"

let formatEmotion emotion =
    match emotion with
    | Fine -> "fine"
    | Happy -> "happy"
    | Sad -> "sad"
    | Angry -> "angry"
    | Inspired -> "inspired"
    | Focused -> "focused"
    | Flirty -> "flirty"
    | Embarrassed -> "embarrassed"
    | Tense -> "tense"

let formatTravelPurpose purpose =
    match purpose with
    | ToWork -> "to work"
    | ToHome -> "home"
    | ToErrand -> "errand"
    | ToLeisure -> "leisure"
    | ToShopping(intent, good) -> $"to {formatIntent intent} shop: {formatGood good}"
    | ToSchool -> "to school"
    | FromSchool -> "from school"
    | ToDaycare -> "to daycare"
    | FromDaycare -> "from daycare"

let formatActivity activity =
    match activity with
    | Sleeping -> "sleeping"
    | MorningRoutine -> "morning routine"
    | Commuting purpose -> $"commuting {formatTravelPurpose purpose}"
    | Working -> "working"
    | Eating -> "eating"
    | Relaxing -> "relaxing"
    | Socializing -> "socializing"
    | Shopping(intent, good) -> $"shopping ({formatIntent intent}: {formatGood good})"
    | AttendingSchool -> "attending school"
    | InDaycare -> "in daycare"
    | Playing -> "playing"
    | Studying -> "studying"
    | CaringForChild _ -> "caring for child"
    | UsingObject interaction -> formatInteraction interaction
    | Cleaning -> "cleaning"
    | Repairing -> "repairing"
    | PracticingSkill skill -> $"practicing {formatSkill skill}"
    | Errand -> "errand"
    | Idle -> "idle"

let formatNeed need =
    match need with
    | Hunger -> "hunger"
    | Energy -> "energy"
    | Social -> "social"
    | Hygiene -> "hygiene"
    | Fun -> "fun"
    | Bladder -> "bladder"
    | Safety -> "safety"
    | Purpose -> "purpose"
    | Learning -> "learning"
    | Comfort -> "comfort"
    | Environment -> "environment"

let placeName (world: World) placeId =
    world.Map.Places
    |> Map.tryFind placeId
    |> Option.map _.Name
    |> Option.defaultValue "unknown place"

let householdName (world: World) householdId =
    world.Households
    |> Map.tryFind householdId
    |> Option.map _.Name
    |> Option.defaultValue "unknown household"

let simName (world: World) simId =
    world.Sims
    |> Map.tryFind simId
    |> Option.map _.Name
    |> Option.defaultValue "unknown sim"

let institutionName (world: World) institutionId =
    world.Institutions
    |> Map.tryFind institutionId
    |> Option.map _.Name
    |> Option.defaultValue "unknown institution"

let formatTravelMode mode =
    match mode with
    | Walk -> "walk"
    | Bike -> "bike"
    | PrivateCar -> "private car"
    | TaxiOrRideshare -> "taxi/rideshare"
    | Bus -> "bus"
    | Tram -> "tram"
    | Metro -> "metro"
    | RegionalRail -> "regional rail"
    | FreightTruck -> "freight truck"
    | EmergencyVehicle -> "emergency vehicle"
    | ServiceVehicle -> "service vehicle"
    | DeliveryVehicle -> "delivery vehicle"
    | SchoolBus -> "school bus"
    | Paratransit -> "paratransit"

let formatTransportPurpose purpose =
    match purpose with
    | WorkTrip -> "work"
    | SchoolTrip -> "school"
    | ShoppingTrip -> "shopping"
    | HealthcareTrip -> "healthcare"
    | CaregivingTrip -> "caregiving"
    | SocialTrip -> "social"
    | RecreationTrip -> "recreation"
    | FreightDeliveryTrip -> "freight"
    | EmergencyResponseTrip -> "emergency"
    | InstitutionalAppointmentTrip -> "appointment"
    | JobInterviewTrip -> "job interview"
    | HousingSearchTrip -> "housing search"
    | SchoolPickupDropoffTrip -> "school pickup/dropoff"

let formatAction (world: World) action =
    match action with
    | PayBillAction amount -> sprintf "pay bill %.0f" (float amount)
    | DelayBillAction amount -> sprintf "delay bill %.0f" (float amount)
    | GoToWorkAction placeId -> $"go to work at {placeName world placeId}"
    | SkipWorkAction -> "skip work"
    | AttendSchoolAction placeId -> $"attend {placeName world placeId}"
    | MissSchoolAction -> "miss school"
    | SeekHelpAction institutionId -> $"seek help from {institutionName world institutionId}"
    | CallPersonAction simId -> $"call {simName world simId}"
    | StartConflictAction simId -> $"start conflict with {simName world simId}"
    | RequestRepairAction _ -> "request repair"
    | MoveHouseholdAction _ -> "move household"
    | FileEvictionAction householdId -> $"file eviction for {householdName world householdId}"
    | NoOpAction -> "do nothing"

let formatReason (world: World) reason =
    match reason with
    | NeedPressure need -> $"need pressure: {formatNeed need}"
    | FinancialPressure -> "financial pressure"
    | HousingInstability -> "housing instability"
    | SocialObligation simId -> $"social obligation to {simName world simId}"
    | CaregivingResponsibility simId -> $"care for {simName world simId}"
    | FearOfConsequence -> "fear of consequence"
    | LongTermAspiration aspiration -> $"aspiration: {aspiration}"
    | HabitualBehavior -> "habit"
    | InstitutionalRequirement institutionId -> $"required by {institutionName world institutionId}"
    | OpportunityAvailable -> "opportunity available"
    | AvoidanceBehavior _ -> "avoidance from memory"
    | RelationshipPressure simId -> $"relationship pressure from {simName world simId}"
    | HealthConstraint -> "health constraint"
    | ScheduleConstraint -> "schedule constraint"
    | LackOfAccess institutionKind -> $"lack of access: {institutionKind}"
    | TrustOrDistrust institutionId -> $"trust/distrust of {institutionName world institutionId}"
    | LegalConstraint -> "legal constraint"
    | CulturalNorm norm -> $"norm: {norm}"
    | PeerInfluence _ -> "peer influence"
    | NoCarAvailable -> "no car available"
    | TransitUnavailable -> "transit unavailable"
    | TransitUnreliable -> "transit unreliable"
    | ParkingTooExpensive -> "parking too expensive"
    | ParkingUnavailable -> "parking unavailable"
    | UnsafeWalkingRoute -> "unsafe walking route"
    | UnsafeBikeRoute -> "unsafe bike route"
    | BadWeather -> "bad weather"
    | HeavyCongestion -> "heavy congestion"
    | HabitualRoute -> "habitual route"
    | FamiliarRoute -> "familiar route"
    | AvoidsHighway -> "avoids highway"
    | AvoidsToll -> "avoids toll"
    | NeedsTripChain -> "needs trip chain"
    | NeedsChildPickup -> "needs child pickup"
    | MobilityLimitation -> "mobility limitation"
    | DeadlinePressure -> "deadline pressure"
    | MissedConnectionRisk -> "missed connection risk"
    | PreviousBadTripMemory _ -> "previous bad trip"
    | FuelCostPressure -> "fuel cost pressure"
    | VehicleMaintenanceIssue -> "vehicle maintenance"
    | RoadClosureKnown -> "known road closure"
    | TransitStrikeOrCancellation -> "transit cancellation"
    | EmergencyPriority -> "emergency priority"
    | FreightRestriction -> "freight restriction"

let formatTransportEvent (world: World) event =
    match event with
    | TripPlanned tripId -> $"trip planned {tripId}"
    | TripStarted tripId -> $"trip started {tripId}"
    | ModeChosen(tripId, mode) -> $"mode chosen for {tripId}: {formatTravelMode mode}"
    | RouteChosen(tripId, _) -> $"route chosen for {tripId}"
    | MovementCompleted(_, tripId) -> $"movement completed for {tripId}"
    | MovementBlocked(_, tripId) -> $"movement blocked for {tripId}"
    | MovementFailed(_, tripId) -> $"movement failed for {tripId}"
    | LaneChanged(_, _, _) -> "lane changed"
    | LaneChangeFailed(_, _, _) -> "lane change failed"
    | ExitMissed(_, nodeId) -> $"exit missed at {nodeId}"
    | RouteReplanned tripId -> $"route replanned for {tripId}"
    | TripDelayed(tripId, delay) -> $"trip delayed {delay}m ({tripId})"
    | TripCanceled tripId -> $"trip canceled {tripId}"
    | TripCompleted tripId -> $"trip completed {tripId}"
    | ArrivedLate(simId, purpose, delay) -> $"{simName world simId} arrived late to {formatTransportPurpose purpose} by {delay}m"
    | MissedTransfer(tripId, _) -> $"missed transfer for {tripId}"
    | TransitVehicleDelayed(routeId, delay) -> $"transit route {routeId} delayed {delay}m"
    | TransitVehicleCrowded routeId -> $"transit route {routeId} crowded"
    | BusBunched routeId -> $"bus bunching on {routeId}"
    | ParkingSearchStarted tripId -> $"parking search started for {tripId}"
    | ParkingFound(tripId, _) -> $"parking found for {tripId}"
    | ParkingFailed tripId -> $"parking failed for {tripId}"
    | IllegalParkingOccurred tripId -> $"illegal parking for {tripId}"
    | CrashOccurred segmentId -> $"crash on {segmentId}"
    | RoadBlocked segmentId -> $"road blocked {segmentId}"
    | SignalFailed nodeId -> $"signal failed at {nodeId}"
    | ConstructionStarted segmentId -> $"construction started on {segmentId}"
    | ConstructionEnded segmentId -> $"construction ended on {segmentId}"
    | EmergencyResponseDelayed(institutionId, delay) -> $"{institutionName world institutionId} response delayed {delay}m"
    | DeliveryDelayed(placeId, delay) -> $"{placeName world placeId} delivery delayed {delay}m"
    | CommutePatternChanged(simId, mode) -> $"{simName world simId} changed commute pattern to {formatTravelMode mode}"
    | HouseholdVehiclePurchased householdId -> $"{householdName world householdId} purchased a vehicle"
    | HouseholdVehicleSold householdId -> $"{householdName world householdId} sold a vehicle"
    | TransitTrustChanged(householdId, delta) ->
        let who = householdId |> Option.map (householdName world) |> Option.defaultValue "citywide"
        sprintf "%s transit trust changed %.2f" who delta
    | RoadConditionDeclined segmentId -> $"road condition declined on {segmentId}"
    | BikeCrashOccurred(simId, _) -> $"{simName world simId} had a bike crash"
    | PedestrianNearMissOccurred(simId, _) -> $"{simName world simId} had a pedestrian near miss"

let formatEvent (world: World) event =
    match event with
    | PersonMoved(_, simId, fromPlace, toPlace) -> $"{simName world simId} moved from {placeName world fromPlace} to {placeName world toPlace}"
    | JobStarted(_, simId, placeId) -> $"{simName world simId} started a job at {placeName world placeId}"
    | JobLost(_, simId, employer) ->
        let source = employer |> Option.map (institutionName world) |> Option.defaultValue "an employer"
        $"{simName world simId} lost a job with {source}"
    | RentIncreased(_, householdId, oldRent, newRent) -> sprintf "%s rent rose from %.0f to %.0f" (householdName world householdId) (float oldRent) (float newRent)
    | BillDue(_, householdId, amount) -> sprintf "%s received a bill for %.0f" (householdName world householdId) (float amount)
    | BillPaid(_, householdId, amount) -> sprintf "%s paid %.0f" (householdName world householdId) (float amount)
    | BillMissed(_, householdId, amount) -> sprintf "%s missed %.0f" (householdName world householdId) (float amount)
    | EvictionFiled(_, householdId, _) -> $"{householdName world householdId} had an eviction filed"
    | EvictionCompleted(_, householdId, _) -> $"{householdName world householdId} was evicted"
    | IllnessOccurred(_, simId) -> $"{simName world simId} got sick"
    | RelationshipChanged _ -> "a relationship changed"
    | ConflictOccurred(_, actor, target, reason) -> $"{simName world actor} conflicted with {simName world target}: {reason}"
    | ChildMissedSchool(_, simId, school) ->
        let source = school |> Option.map (institutionName world) |> Option.defaultValue "school"
        $"{simName world simId} missed {source}"
    | SchoolDayCompleted(_, simId, school) ->
        let source = school |> Option.map (institutionName world) |> Option.defaultValue "school"
        $"{simName world simId} completed a day at {source}"
    | CrimeOccurred(_, neighborhoodId, description) ->
        let neighborhood = world.Neighborhoods |> Map.tryFind neighborhoodId |> Option.map _.Name |> Option.defaultValue "a neighborhood"
        $"{description} in {neighborhood}"
    | PoliceInteractionOccurred(_, simId, institutionId) -> $"{simName world simId} interacted with {institutionName world institutionId}"
    | HospitalVisitOccurred(_, simId, institutionId) -> $"{simName world simId} visited {institutionName world institutionId}"
    | BusinessOpened(_, placeId) -> $"{placeName world placeId} opened"
    | BusinessClosed(_, placeId) -> $"{placeName world placeId} closed"
    | PolicyPassed(_, policy) -> $"policy passed: {policy}"
    | ServiceCapacityChanged(_, institutionId, oldCapacity, newCapacity) -> $"{institutionName world institutionId} capacity changed {oldCapacity} -> {newCapacity}"
    | NeighborhoodReputationChanged(_, neighborhoodId, delta) ->
        let neighborhood = world.Neighborhoods |> Map.tryFind neighborhoodId |> Option.map _.Name |> Option.defaultValue "neighborhood"
        sprintf "%s reputation changed by %.2f" neighborhood delta
    | HouseholdBudgetChanged(_, householdId, delta) -> sprintf "%s budget changed %.0f" (householdName world householdId) (float delta)
    | TransportEventOccurred(_, transportEvent) -> formatTransportEvent world transportEvent
    | event -> sprintf "%A" event

let printRoutePreview world =
    printfn "Routes"

    world.Sims
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.choose (fun sim ->
        sim.Job
        |> Option.bind (fun job -> Simulation.MapGraph.findRoute world.Map sim.Home job.Workplace)
        |> Option.map (fun route -> sim.Name, route))
    |> Seq.iter (fun (name, route) ->
        printfn "  %-12s %s" name (Simulation.MapGraph.describeRoute world.Map route))

    let homes =
        world.Map.Places
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun place -> place.Kind = Residence)
        |> Seq.tryHead

    let parks =
        world.Map.Places
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun place -> place.Kind = Park)
        |> Seq.tryHead

    match homes, parks with
    | Some home, Some park ->
        Simulation.MapGraph.findRoute world.Map home.Id park.Id
        |> Option.iter (fun route -> printfn "  %-12s %s" "Off-road" (Simulation.MapGraph.describeRoute world.Map route))
    | _ -> ()

    match homes with
    | Some home ->
        [ Groceries, NeedPurchase, "Need shop"
          Electronics, WantPurchase, "Want shop" ]
        |> List.iter (fun (good, intent, label) ->
            world.Sims
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.tryHead
            |> Option.bind (fun sim -> Simulation.Economy.findShoppingDestination world.Map sim home.Id good intent)
            |> Option.bind (fun (placeId, _) -> Simulation.MapGraph.findRoute world.Map home.Id placeId)
            |> Option.iter (fun route -> printfn "  %-12s %s" label (Simulation.MapGraph.describeRoute world.Map route)))
    | None -> ()

let printEconomyPreview world =
    printfn ""
    printfn "Commerce"

    Simulation.Economy.commercialSummary world.Map
    |> List.iter (printfn "  %s")

let formatSeverity severity =
    match severity with
    | Info -> "info"
    | Warning -> "warning"
    | Critical -> "critical"

let formatScenario scenario =
    match scenario with
    | StrugglingInnerRingSuburb -> "Struggling Inner-Ring Suburb"
    | FastGrowingSunbeltSuburb -> "Fast-Growing Sunbelt Suburb"
    | OldIndustrialRiverCity -> "Old Industrial River City"
    | CollegeTownScenario -> "College Town"
    | RuralCountySeat -> "Rural County Seat"
    | CustomScenario name -> name

let formatValidationStatus status =
    match status with
    | Passed -> "passed"
    | Repaired -> "repaired"
    | IntentionalConstraint -> "constraint"
    | ValidationStatus.Failed -> "failed"

let formatRiskArea area =
    match area with
    | PoliticsAndGovernance -> "politics"
    | LandOwnership -> "ownership"
    | HousingAffordability -> "affordability"
    | CapitalAndFinance -> "capital"
    | TransportBehavior -> "transport"
    | ServiceQuality -> "services"
    | Psychology -> "psychology"
    | RelationshipDepth -> "relationships"
    | Inequality -> "inequality"
    | MaintenanceAndDecay -> "maintenance"
    | TimeCompression -> "time"
    | AgentConflict -> "conflict"

let printCityDashboard world =
    let city = world.City
    let indicators = city.Indicators

    printfn ""
    printfn "City"
    printfn "  %s: population=%i jobs=%i unemployment=%.0f%% treasury=%.0f" city.Name indicators.Population indicators.Jobs (indicators.Unemployment * 100.0) (float city.Budget.Treasury)
    printfn "  RCI demand: R=%.2f C=%.2f I=%.2f" city.Demand.Residential city.Demand.Commercial city.Demand.Industrial
    printfn "  Land=%.2f desirability=%.2f pollution=%.2f crime=%.2f fire=%.2f traffic=%.2f" indicators.AverageLandValue indicators.AverageDesirability indicators.Pollution indicators.Crime indicators.FireRisk indicators.Traffic
    printfn "  Budget/month: income=%.0f expenses=%.0f" (float city.Budget.MonthlyIncome) (float city.Budget.MonthlyExpenses)

    printfn "  Utilities"
    city.Utilities
    |> List.iter (fun utility -> printfn "    %-16s %.0f / %.0f" utility.Name utility.Used utility.Capacity)

    printfn "  Advisors"
    if List.isEmpty city.Advisors then
        printfn "    No urgent issues."
    else
        city.Advisors
        |> List.truncate 6
        |> List.iter (fun advisor -> printfn "    [%s] %s: %s" (formatSeverity advisor.Severity) advisor.Department advisor.Message)

let printLifeDashboard world =
    printfn ""
    printfn "Households"

    world.Households
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.iter (fun household ->
        printfn
            "  %-16s funds=%.0f bills=%.0f clean=%.2f objects=%i"
            household.Name
            (float household.Funds)
            (float household.BillsDue)
            household.Cleanliness
            household.Objects.Length)

    printfn "Life Sim"

    world.Sims
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.iter (fun sim ->
        let topSkill =
            sim.Skills
            |> Map.toSeq
            |> Seq.sortByDescending (fun (_, skill) -> skill.Level, skill.Experience)
            |> Seq.tryHead
            |> Option.map (fun (kind, skill) -> sprintf "%s %i" (formatSkill kind) skill.Level)
            |> Option.defaultValue "no skills"

        let topMoodlet =
            sim.Moodlets
            |> List.sortByDescending _.Strength
            |> List.tryHead
            |> Option.map _.Name
            |> Option.defaultValue "none"

        let aspiration =
            sim.Aspiration
            |> Option.map (fun aspiration -> sprintf "%A %.0f%%" aspiration.Kind (aspiration.Progress * 100.0))
            |> Option.defaultValue "none"

        printfn
            "  %-12s emotion=%-11s topSkill=%-16s moodlet=%-22s aspiration=%s"
            sim.Name
            (formatEmotion sim.Emotion)
            topSkill
            topMoodlet
            aspiration)

let printDiagnosticsDashboard (world: World) =
    printfn ""
    printfn "Simulation Diagnostics"
    printfn "  Overall fragility=%.2f" world.Diagnostics.OverallFragility

    world.Diagnostics.Risks
    |> List.truncate 6
    |> List.iter (fun (risk: SimulationRisk) ->
        printfn
            "  [%s] %-14s score=%.2f %s"
            (formatSeverity risk.Severity)
            (formatRiskArea risk.Area)
            risk.Score
            risk.Message)

let printWorldGenerationDashboard (world: World) =
    let report = world.GenerationReport

    printfn ""
    printfn "World Generation"
    printfn "  Scenario: %s seed=%i" (formatScenario report.Scenario) report.Seed
    printfn "  Geography: %A buildable=%.0f%% floodRisk=%.0f%% openSpace=%.0f%%" world.Geography.Terrain (world.Geography.BuildableLandRatio * 100.0) (world.Geography.FloodRisk * 100.0) (world.Geography.OpenSpaceRatio * 100.0)
    printfn "  Settlements"
    world.Settlements
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.iter (fun settlement ->
        printfn
            "    %-22s %-18A %-18A popTarget=%i jobs=%i walk=%.2f transit=%.2f parkingDependence=%.2f"
            settlement.Name
            settlement.SettlementType
            settlement.Archetype
            settlement.PopulationTarget
            settlement.EmploymentTarget
            settlement.Walkability
            settlement.TransitViability
            settlement.ParkingDependence)

    printfn "  Structure: districts=%i blocks=%i parcels=%i generatedJobs=%i" world.Districts.Count world.Blocks.Count world.City.Parcels.Count world.GeneratedJobs.Count

    report.GeneratedSummary
    |> List.truncate 3
    |> List.iter (fun line -> printfn "    %s" line)

    printfn "  Validation"
    if report.Findings.IsEmpty then
        printfn "    No validation issues."
    else
        report.Findings
        |> List.truncate 5
        |> List.iter (fun finding -> printfn "    [%s] %s" (formatValidationStatus finding.Status) finding.Message)

let printTransportDashboard world =
    let metrics = world.Transport.Metrics

    printfn ""
    printfn "Transport"
    printfn
        "  lanes=%i intersections=%i trips=%i vehicles=%i congestion=%.2f reliability=%.2f parkingPressure=%.2f transitTrust=%.2f"
        world.Transport.Lanes.Count
        world.Transport.Intersections.Count
        world.Transport.Trips.Count
        world.Transport.Vehicles.Count
        metrics.AverageCongestion
        metrics.AverageTravelReliability
        metrics.AverageParkingPressure
        metrics.TransitTrust

    printfn
        "  today: late=%i failedMerges=%i missedTransfers=%i parkingFailures=%i emergencyRisk=%.2f freightReliability=%.2f"
        metrics.LateArrivalsToday
        metrics.FailedLaneChangesToday
        metrics.MissedTransfersToday
        metrics.ParkingFailuresToday
        metrics.EmergencyResponseRisk
        metrics.FreightReliability

    printfn "  Access"
    world.Transport.AccessByNeighborhood
    |> Map.toSeq
    |> Seq.iter (fun (neighborhoodId, access) ->
        let neighborhood = world.Neighborhoods |> Map.tryFind neighborhoodId |> Option.map _.Name |> Option.defaultValue "unknown"
        printfn
            "    %-24s jobs=%.2f school=%.2f food=%.2f transit=%.2f walk=%.2f bike=%.2f opportunity=%.2f"
            neighborhood
            access.JobAccess
            access.SchoolAccess
            access.FoodAccess
            access.TransitReliability
            access.WalkSafety
            access.BikeSafety
            access.OpportunityAccess)

    let recentTrips =
        world.Transport.Trips
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.sortByDescending (fun (trip: TransportTrip) ->
            match trip.Status with
            | TripStatus.InProgress -> 3
            | TripStatus.Planned -> 2
            | TripStatus.Completed -> 1
            | TripStatus.Canceled
            | TripStatus.Failed -> 0)
        |> Seq.truncate 4
        |> Seq.toList

    if not recentTrips.IsEmpty then
        printfn "  Recent trips"

        recentTrips
        |> List.iter (fun trip ->
            let actor =
                trip.PersonId
                |> Option.map (simName world)
                |> Option.orElse (trip.HouseholdId |> Option.map (householdName world))
                |> Option.defaultValue "system"

            let mode = trip.ChosenMode |> Option.map formatTravelMode |> Option.defaultValue "none"
            let route = trip.CurrentRoute |> Option.map (fun route -> sprintf "%im reliability=%.2f stress=%.2f" route.ExpectedMinutes route.Reliability route.Stress) |> Option.defaultValue "no route"
            let reasons = trip.ModeChoiceReasons |> List.map (formatReason world) |> String.concat ", "

            printfn "    %-14s %-9s %-18s %s why=%s" actor mode (formatTransportPurpose trip.Purpose) route reasons)

let printCausalityDashboard world =
    printfn ""
    printfn "Causality"
    printfn
        "  tick=%i events=%i decisions=%i memories=%i relationships=%i groups=%i institutions=%i"
        world.Meta.Tick
        world.Meta.EventLog.Length
        world.Meta.Decisions.Length
        world.Memories.Count
        world.Relationships.Count
        world.Groups.Count
        world.Institutions.Count

    printfn "  Housing"
    world.HousingUnits
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.iter (fun unit ->
        let neighborhood = world.Neighborhoods |> Map.tryFind unit.Neighborhood |> Option.map _.Name |> Option.defaultValue "unknown"
        let households = unit.Occupants |> Seq.map (householdName world) |> String.concat ", "
        let rent = unit.RentMonthly |> Option.map (fun value -> sprintf "%.0f" (float value)) |> Option.defaultValue "n/a"
        printfn "    %-24s rent=%s condition=%.2f evictionRisk=%.2f households=%s" neighborhood rent unit.Condition unit.EvictionRisk households)

    printfn "  Recent decisions"
    if List.isEmpty world.Meta.Decisions then
        printfn "    No agent decisions recorded yet."
    else
        world.Meta.Decisions
        |> List.truncate 4
        |> List.iter (fun decision ->
            let actor =
                decision.Actor
                |> Option.map (simName world)
                |> Option.orElse (decision.Household |> Option.map (householdName world))
                |> Option.defaultValue "system"

            let reasons =
                decision.Reasons
                |> List.map (formatReason world)
                |> String.concat ", "

            printfn "    %-18s -> %-18s why=%s" actor (formatAction world decision.ChosenAction) reasons)

    printfn "  Recent events"
    if List.isEmpty world.Meta.EventLog then
        printfn "    No events recorded yet."
    else
        world.Meta.EventLog
        |> List.truncate 5
        |> List.iter (fun event -> printfn "    %s" (formatEvent world event))

let describeLocation (world: World) (sim: Sim) =
    match sim.Location with
    | AtPlace placeId ->
        world.Map.Places
        |> Map.tryFind placeId
        |> Option.map _.Name
        |> Option.defaultValue "Unknown place"
    | InTransit trip ->
        let destination =
            world.Map.Places
            |> Map.tryFind trip.Destination
            |> Option.map _.Name
            |> Option.defaultValue "Unknown destination"

        $"en route to {destination} ({trip.RemainingMinutes}m left)"

let printSnapshot (world: World) =
    printfn ""
    printfn $"Day {world.Day}, {formatTime world.MinuteOfDay}"

    printfn
        "  City        pop=%i jobs=%i treasury=%.0f RCI=(%.2f, %.2f, %.2f) land=%.2f pollution=%.2f fragility=%.2f events=%i memories=%i"
        world.City.Indicators.Population
        world.City.Indicators.Jobs
        (float world.City.Budget.Treasury)
        world.City.Demand.Residential
        world.City.Demand.Commercial
        world.City.Demand.Industrial
        world.City.Indicators.AverageLandValue
        world.City.Indicators.Pollution
        world.Diagnostics.OverallFragility
        world.Meta.EventLog.Length
        world.Memories.Count

    world.Sims
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.iter (fun (sim: Sim) ->
        let hunger = sim.Needs[Hunger].Value
        let energy = sim.Needs[Energy].Value
        let social = sim.Needs[Social].Value

        printfn
            "  %-12s %-34s emotion=%-10s happy=%.2f hunger=%.2f energy=%.2f social=%.2f @ %s"
            sim.Name
            (formatActivity sim.Activity)
            (formatEmotion sim.Emotion)
            sim.Happiness
            hunger
            energy
            social
            (describeLocation world sim))

let rec advanceTicks tickMinutes count world =
    if count <= 0 then
        world
    else
        advanceTicks tickMinutes (count - 1) (tick tickMinutes world)

let rec runSnapshots tickMinutes remaining world =
    printSnapshot world

    if remaining > 0 then
        runSnapshots tickMinutes (remaining - 1) (advanceTicks tickMinutes 4 world)
    else
        world

type RunnerOptions =
    { Seed: int
      Snapshots: int
      TickMinutes: int }

let defaultOptions =
    { Seed = 1337
      Snapshots = 18
      TickMinutes = 15 }

let private parseInt (optionName: string) (value: string) (fallback: int) =
    match System.Int32.TryParse value with
    | true, parsed when parsed > 0 -> parsed
    | _ ->
        printfn "Ignoring invalid %s value '%s'; using %i." optionName value fallback
        fallback

let parseArgs (argv: string array) =
    let rec loop (options: RunnerOptions) args =
        match args with
        | [] -> options
        | "--seed" :: value :: rest ->
            loop { options with Seed = parseInt "--seed" value options.Seed } rest
        | "--snapshots" :: value :: rest ->
            loop { options with Snapshots = parseInt "--snapshots" value options.Snapshots } rest
        | "--tick-minutes" :: value :: rest ->
            loop { options with TickMinutes = parseInt "--tick-minutes" value options.TickMinutes } rest
        | unknown :: rest ->
            printfn "Ignoring unknown argument '%s'." unknown
            loop options rest

    argv |> Array.toList |> loop defaultOptions

[<EntryPoint>]
let main argv =
    let options = parseArgs argv
    let world = Juniper.createWorld options.Seed

    printfn "RealSim Juniper scenario"
    printfn "  seed=%i snapshots=%i tickMinutes=%i" options.Seed options.Snapshots options.TickMinutes

    printWorldGenerationDashboard world
    printCityDashboard world
    printLifeDashboard world
    printTransportDashboard world
    printCausalityDashboard world
    printRoutePreview world
    printEconomyPreview world

    let finalWorld = runSnapshots options.TickMinutes options.Snapshots world

    printfn ""
    printfn "Simulation complete."
    printfn "  finalDay=%i finalTime=%s events=%i memories=%i" finalWorld.Day (formatTime finalWorld.MinuteOfDay) finalWorld.Meta.EventLog.Length finalWorld.Memories.Count
    0
