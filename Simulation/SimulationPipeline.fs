namespace Simulation

open System
open System.Security.Cryptography
open System.Text
open Simulation.Domain
open Simulation.Measures

module SimulationPipeline =
    let private stableGuid parts =
        let text = String.concat "|" parts
        let bytes = Encoding.UTF8.GetBytes text
        let hash = SHA256.HashData bytes
        let guidBytes = Array.zeroCreate<byte> 16
        Array.Copy(hash, guidBytes, 16)
        Guid(guidBytes)

    let private eventId seed tick label index =
        EventId(stableGuid [ string seed; string tick; label; string index ])

    let private memoryId seed tick label index =
        MemoryId(stableGuid [ "memory"; string seed; string tick; label; string index ])

    let private intentId seed tick label index =
        stableGuid [ "intent"; string seed; string tick; label; string index ]

    let private partitionForHousehold (HouseholdId id) =
        let idStr = id.ToString("N")
        $"household:%s{idStr}"

    let private isMonthlyRentMoment world =
        world.Day % 30 = 5 && world.MinuteOfDay = 6 * 60

    let private relationshipDimensionsDefault =
        { Affection = 0.0
          Trust = 0.0
          Attraction = 0.0
          Respect = 0.0
          Fear = 0.0
          Obligation = 0.0
          Dependence = 0.0
          Resentment = 0.0
          Familiarity = 0.0
          PowerImbalance = 0.0
          Loyalty = 0.0
          Reputation = 0.0
          Conflict = 0.0 }

    let rebuildIndexes world =
        let personIdsByHousehold =
            world.Sims
            |> Map.toSeq
            |> Seq.groupBy (fun (_, sim) -> sim.Household)
            |> Seq.map (fun (householdId, sims) -> householdId, sims |> Seq.map fst |> Seq.toList)
            |> Map.ofSeq

        let householdNeighborhood household =
            world.HousingUnits
            |> Map.toSeq
            |> Seq.tryFind (fun (_, unit) -> Set.contains household.Id unit.Occupants)
            |> Option.map (fun (_, unit) -> unit.Neighborhood)

        let personIdsByNeighborhood =
            world.Households
            |> Map.toSeq
            |> Seq.choose (fun (_, household) -> householdNeighborhood household |> Option.map (fun n -> n, household.Members))
            |> Seq.collect (fun (neighborhoodId, members) -> members |> Seq.map (fun personId -> neighborhoodId, personId))
            |> Seq.groupBy fst
            |> Seq.map (fun (neighborhoodId, members) -> neighborhoodId, members |> Seq.map snd |> Seq.toList)
            |> Map.ofSeq

        let relationshipIdsByPerson =
            world.Relationships
            |> Map.toSeq
            |> Seq.collect (fun (relationshipId, edge) -> [ edge.From, relationshipId; edge.Toward, relationshipId ])
            |> Seq.groupBy fst
            |> Seq.map (fun (personId, edges) -> personId, edges |> Seq.map snd |> Seq.distinct |> Seq.toList)
            |> Map.ofSeq

        let groupIdsByPerson =
            world.Groups
            |> Map.toSeq
            |> Seq.collect (fun (groupId, group) -> group.Members |> Seq.map (fun personId -> personId, groupId))
            |> Seq.groupBy fst
            |> Seq.map (fun (personId, groups) -> personId, groups |> Seq.map snd |> Seq.toList)
            |> Map.ofSeq

        let unitIdsByNeighborhood =
            world.HousingUnits
            |> Map.toSeq
            |> Seq.groupBy (fun (_, unit) -> unit.Neighborhood)
            |> Seq.map (fun (neighborhoodId, units) -> neighborhoodId, units |> Seq.map fst |> Seq.toList)
            |> Map.ofSeq

        let institutionIdsByNeighborhood =
            world.Institutions
            |> Map.toSeq
            |> Seq.groupBy (fun (_, institution) -> institution.Neighborhood)
            |> Seq.map (fun (neighborhoodId, institutions) -> neighborhoodId, institutions |> Seq.map fst |> Seq.toList)
            |> Map.ofSeq

        let studentIdsBySchool =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (fun (personId, sim) ->
                sim.School
                |> Option.bind (fun enrollment ->
                    world.Institutions
                    |> Map.toSeq
                    |> Seq.tryFind (fun (_, institution) -> institution.Place = Some enrollment.School)
                    |> Option.map (fun (institutionId, _) -> institutionId, personId)))
            |> Seq.groupBy fst
            |> Seq.map (fun (schoolId, students) -> schoolId, students |> Seq.map snd |> Seq.toList)
            |> Map.ofSeq

        { PersonIdsByHousehold = personIdsByHousehold
          PersonIdsByNeighborhood = personIdsByNeighborhood
          RelationshipIdsByPerson = relationshipIdsByPerson
          GroupIdsByPerson = groupIdsByPerson
          UnitIdsByNeighborhood = unitIdsByNeighborhood
          InstitutionIdsByNeighborhood = institutionIdsByNeighborhood
          StudentIdsBySchool = studentIdsBySchool }

    let private partitionForTrip (trip: TransportTrip) =
        match trip.HouseholdId with
        | Some householdId -> partitionForHousehold householdId |> PartitionId
        | None ->
            match trip.PersonId with
            | Some (SimId id) ->
                let idStr = id.ToString("N")
                PartitionId($"person:%s{idStr}")
            | None -> PartitionId "system"

    let rebuildRuntimeIndexes world =
        let personIdsByIndex =
            world.Sims
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.sort
            |> Seq.toArray

        let personIndexById =
            personIdsByIndex
            |> Array.mapi (fun index simId -> simId, PersonIndex index)
            |> Map.ofArray

        let householdIdsByIndex =
            world.Households
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.sort
            |> Seq.toArray

        let householdIndexById =
            householdIdsByIndex
            |> Array.mapi (fun index householdId -> householdId, HouseholdIndex index)
            |> Map.ofArray

        let laneIdsByIndex =
            world.Transport.Lanes
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.sort
            |> Seq.toArray

        let laneIndexById =
            laneIdsByIndex
            |> Array.mapi (fun index laneId -> laneId, LaneIndex index)
            |> Map.ofArray

        let needValue needKind sim =
            sim.Needs
            |> Map.tryFind needKind
            |> Option.map _.Value
            |> Option.defaultValue 0.0

        let needsByPersonIndex =
            personIdsByIndex
            |> Array.map (fun simId ->
                let sim = world.Sims[simId]

                { Hunger = needValue Hunger sim
                  Energy = needValue Energy sim
                  Social = needValue Social sim
                  Comfort = needValue Comfort sim })

        let relationshipCountsByPerson =
            world.Relationships
            |> Map.toSeq
            |> Seq.collect (fun (_, edge) -> [ edge.From; edge.Toward ])
            |> Seq.countBy id
            |> Map.ofSeq

        let relationshipsByPersonIndex, _ =
            (0, personIdsByIndex)
            ||> Array.mapFold (fun offset simId ->
                let count = relationshipCountsByPerson |> Map.tryFind simId |> Option.defaultValue 0
                let range : RelationshipIndexRange = { Start = offset; Length = count }
                range, offset + count)

        let lanesByIndex : LaneRuntimeState array =
            laneIdsByIndex
            |> Array.map (fun laneId ->
                let lane = world.Transport.Lanes[laneId]

                { Lane = lane.Id
                  Segment = lane.SegmentId
                  Density = lane.CurrentDensity
                  SpeedKph = lane.CurrentSpeedKph
                  QueueLength = lane.QueueLength
                  Blocked = lane.Blocked })

        let intersectionIncomingLaneRanges : Map<RoadNodeId, LaneIndexRange> =
            world.Transport.Intersections
            |> Map.toSeq
            |> Seq.map (fun (nodeId, intersection) ->
                let ordered =
                    intersection.IncomingLanes
                    |> Set.toArray
                    |> Array.sort

                let indices =
                    ordered
                    |> Array.choose (fun laneId -> laneIndexById |> Map.tryFind laneId |> Option.map (fun (LaneIndex index) -> index))

                let range =
                    if indices.Length = 0 then
                        { Start = 0; Length = 0 }
                    else
                        { Start = Array.min indices; Length = indices.Length }

                nodeId, range)
            |> Map.ofSeq

        let tripsByPartition =
            world.Transport.Trips
            |> Map.toSeq
            |> Seq.groupBy (fun (_, trip) -> partitionForTrip trip)
            |> Seq.map (fun (partition, trips) ->
                partition,
                trips
                |> Seq.map fst
                |> Seq.sort
                |> Seq.toArray)
            |> Map.ofSeq

        { PersonIndexById = personIndexById
          PersonIdsByIndex = personIdsByIndex
          HouseholdIndexById = householdIndexById
          HouseholdIdsByIndex = householdIdsByIndex
          LaneIndexById = laneIndexById
          LaneIdsByIndex = laneIdsByIndex
          NeedsByPersonIndex = needsByPersonIndex
          RelationshipsByPersonIndex = relationshipsByPersonIndex
          LanesByIndex = lanesByIndex
          IntersectionIncomingLaneRanges = intersectionIncomingLaneRanges
          TripsByPartition = tripsByPartition
          RouteCache = world.Runtime.RouteCache
          TravelTimeCache = world.Runtime.TravelTimeCache
          CacheVersion = world.Runtime.CacheVersion }

    let private monthlySystemEvents world =
        if isMonthlyRentMoment world then
            world.Households
            |> Map.toSeq
            |> Seq.choose (fun (householdId, household) ->
                household.RentMonthly
                |> Option.map (fun rent -> BillDue(eventId world.Meta.Seed world.Meta.Tick "rent-due" (householdId.GetHashCode()), householdId, rent)))
            |> Seq.toList
        else
            []

    let private attendanceEvents world =
        world.Sims
        |> Map.toSeq
        |> Seq.choose (fun (simId, sim) ->
            match sim.School, sim.Activity with
            | Some enrollment, AttendingSchool when world.MinuteOfDay = enrollment.EndMinute ->
                let schoolInstitution =
                    world.Institutions
                    |> Map.toSeq
                    |> Seq.tryFind (fun (_, institution) -> institution.Place = Some enrollment.School)
                    |> Option.map fst

                Some(SchoolDayCompleted(eventId world.Meta.Seed world.Meta.Tick "school-complete" (simId.GetHashCode()), simId, schoolInstitution))
            | Some enrollment, _ when world.MinuteOfDay > enrollment.StartMinute + 90 && world.MinuteOfDay < enrollment.EndMinute && sim.Location = AtPlace sim.Home ->
                let schoolInstitution =
                    world.Institutions
                    |> Map.toSeq
                    |> Seq.tryFind (fun (_, institution) -> institution.Place = Some enrollment.School)
                    |> Option.map fst

                Some(ChildMissedSchool(eventId world.Meta.Seed world.Meta.Tick "school-missed" (simId.GetHashCode()), simId, schoolInstitution))
            | _ -> None)
        |> Seq.toList

    let private generateSystemEvents world =
        let transportEvents =
            world.Transport.RecentEvents
            |> List.mapi (fun index event -> TransportEventOccurred(eventId world.Meta.Seed world.Meta.Tick "transport" index, event))

        monthlySystemEvents world @ attendanceEvents world @ transportEvents

    let private householdIntent world index householdId household =
        let boundedAlternatives =
            [ household.RentMonthly |> Option.map DelayBillAction
              household.RentMonthly |> Option.map PayBillAction
              Some NoOpAction ]
            |> List.choose id
            |> List.truncate 3

        if household.BillsDue > 0m && world.MinuteOfDay % 60 = 0 then
            let canPay = household.Funds >= household.BillsDue
            let action = if canPay then PayBillAction household.BillsDue else DelayBillAction household.BillsDue
            let reasons =
                [ FinancialPressure
                  if not canPay then HousingInstability
                  if household.Stability < 0.45 then FearOfConsequence ]

            Some
                { Id = intentId world.Meta.Seed world.Meta.Tick "household-bill" index
                  PartitionKey = partitionForHousehold householdId
                  Decision =
                    { Actor = None
                      Household = Some householdId
                      ChosenAction = action
                      RejectedAlternatives = boundedAlternatives |> List.filter ((<>) action)
                      Reasons = reasons
                      ExpectedConsequences =
                        if canPay then
                            [ "Household funds fall, but housing stability improves." ]
                        else
                            [ "Unpaid bills increase instability and may become eviction pressure." ]
                      Confidence = if canPay then 0.86 else 0.62
                      Urgency = 0.90
                      TimeCostMinutes = 10
                      MoneyCost = if canPay then household.BillsDue else 0m
                      SocialCost = if canPay then 0.05 else 0.25
                      Risk = if canPay then 0.08 else 0.55 } }
        else
            None

    let private generateIntents world =
        world.Households
        |> Map.toSeq
        |> Seq.sortBy (fun (HouseholdId id, _) -> id)
        |> Seq.mapi (fun index (householdId, household) -> householdIntent world index householdId household)
        |> Seq.choose id
        |> Seq.toList

    let private resolveIntents intents =
        intents
        |> List.sortBy (fun intent -> intent.PartitionKey, intent.Id)
        |> List.distinctBy (fun intent -> intent.Decision.Household, intent.Decision.Actor)

    let private eventsFromIntent world index intent =
        match intent.Decision.Household, intent.Decision.ChosenAction with
        | Some householdId, PayBillAction amount ->
            [ BillPaid(eventId world.Meta.Seed world.Meta.Tick "bill-paid" index, householdId, amount)
              HouseholdBudgetChanged(eventId world.Meta.Seed world.Meta.Tick "budget-paid" index, householdId, -amount) ]
        | Some householdId, DelayBillAction amount ->
            [ BillMissed(eventId world.Meta.Seed world.Meta.Tick "bill-missed" index, householdId, amount) ]
        | _ -> []

    let private eventIdOf event =
        match event with
        | PersonMoved(id, _, _, _)
        | JobStarted(id, _, _)
        | JobLost(id, _, _)
        | RentIncreased(id, _, _, _)
        | BillDue(id, _, _)
        | BillPaid(id, _, _)
        | BillMissed(id, _, _)
        | EvictionFiled(id, _, _)
        | EvictionCompleted(id, _, _)
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

    let private memoryFromEvent world index event =
        let source = eventIdOf event
        let baseMemory salience weight tags people institutions neighborhood effects =
            { Id = memoryId world.Meta.Seed world.Meta.Tick "event-memory" index
              SourceEvent = source
              Day = world.Day
              Minute = world.MinuteOfDay
              EmotionalWeight = weight
              Salience = salience
              Tags = Set.ofList tags
              PeopleInvolved = Set.ofList people
              InstitutionsInvolved = Set.ofList institutions
              Neighborhood = neighborhood
              Effects = Set.ofList effects
              DecayPerDay = if salience = Traumatic || salience = Formative then 0.002 else 0.020 }

        match event with
        | BillMissed(_, householdId, _) ->
            let people = world.Meta.Indexes.PersonIdsByHousehold |> Map.tryFind householdId |> Option.defaultValue []
            Some(baseMemory Important 0.65 [ "bill"; "financial-stress" ] people [] None [ AffectsFear; AffectsAvoidance ])
        | BillPaid(_, householdId, _) ->
            let people = world.Meta.Indexes.PersonIdsByHousehold |> Map.tryFind householdId |> Option.defaultValue []
            Some(baseMemory Notable 0.25 [ "bill"; "stability" ] people [] None [ AffectsTrust None ])
        | RentIncreased(_, householdId, _, _) ->
            let people = world.Meta.Indexes.PersonIdsByHousehold |> Map.tryFind householdId |> Option.defaultValue []
            Some(baseMemory Important 0.55 [ "rent"; "housing" ] people [] None [ AffectsFear; AffectsResentment None ])
        | ChildMissedSchool(_, simId, institution) ->
            Some(baseMemory Notable 0.42 [ "school"; "absence" ] [ simId ] (institution |> Option.toList) None [ AffectsAmbition; AffectsFear ])
        | SchoolDayCompleted(_, simId, institution) ->
            Some(baseMemory Trivial 0.12 [ "school"; "routine" ] [ simId ] (institution |> Option.toList) None [ AffectsAttachment None ])
        | TransportEventOccurred(_, ArrivedLate(simId, purpose, delay)) ->
            let weight = if delay >= 20 then 0.50 else 0.30
            Some(baseMemory Important weight [ "transport"; "late-arrival"; string purpose ] [ simId ] [] None [ AffectsFear; AffectsAvoidance ])
        | TransportEventOccurred(_, ParkingFailed tripId) ->
            let people =
                world.Transport.Trips
                |> Map.tryFind tripId
                |> Option.bind _.PersonId
                |> Option.toList

            Some(baseMemory Notable 0.34 [ "transport"; "parking"; "failed-trip-friction" ] people [] None [ AffectsAvoidance ])
        | TransportEventOccurred(_, LaneChangeFailed(vehicleId, _, _)) ->
            let people =
                world.Transport.Vehicles
                |> Map.tryFind vehicleId
                |> Option.bind (fun vehicle -> world.Transport.Trips |> Map.tryFind vehicle.Trip)
                |> Option.bind _.PersonId
                |> Option.toList

            Some(baseMemory Notable 0.28 [ "transport"; "merge"; "near-miss" ] people [] None [ AffectsFear ])
        | TransportEventOccurred(_, TransitVehicleDelayed(routeId, _)) ->
            let institution =
                world.Institutions
                |> Map.toSeq
                |> Seq.tryFind (fun (_, institution) -> institution.Kind = TransitInstitution)
                |> Option.map fst

            Some(baseMemory Notable 0.25 [ "transport"; "transit-delay"; string routeId ] [] (institution |> Option.toList) None [ AffectsTrust institution ])
        | BuildingDestroyed(_, _, _, _, reason, _) ->
            Some(baseMemory Important 0.42 [ "built-environment"; "building-destroyed"; string reason ] [] [] None [ AffectsAttachment None ])
        | HouseholdsDisplaced(_, _, _, households) ->
            let people =
                households
                |> List.collect (fun householdId ->
                    world.Households
                    |> Map.tryFind householdId
                    |> Option.map (fun household -> household.Members |> Set.toList)
                    |> Option.defaultValue [])

            Some(baseMemory Traumatic 0.70 [ "housing"; "displacement" ] people [] None [ AffectsFear; AffectsAttachment None ])
        | RoadBuilt(_, _, segment, _, _) ->
            Some(baseMemory Trivial 0.12 [ "transport"; "road-built"; string segment.Id ] [] [] None [ AffectsAttachment None ])
        | _ -> None

    let private appendMemoryToPeople (memory: Memory) (world: World) =
        let sims =
            memory.PeopleInvolved
            |> Set.fold (fun (sims: Map<SimId, Sim>) simId ->
                sims
                |> Map.change simId (Option.map (fun (sim: Sim) ->
                    { sim with Memories = (memory.Id :: sim.Memories) |> List.truncate 50 }))) world.Sims

        { world with Sims = sims }

    let private chargeCityBudget cost world =
        if cost <= 0m then
            world
        else
            { world with
                City =
                    { world.City with
                        Budget =
                            { world.City.Budget with
                                Treasury = world.City.Budget.Treasury - cost } } }

    let private addStreetEventId eventId world =
        { world with
            Street =
                { world.Street with
                    RecentEventIds = (eventId :: world.Street.RecentEventIds) |> List.truncate 100 } }

    let private heatRank =
        function
        | NoHeat -> 0
        | LowHeat -> 1
        | ModerateHeat -> 2
        | HighHeat -> 3
        | CitywideAlert -> 4

    let private updateActor (actorId: ActorId) (updater: Actor -> Actor) (world: World) =
        { world with
            Street =
                { world.Street with
                    Actors =
                        world.Street.Actors
                        |> Map.change actorId (Option.map updater) } }

    let private updateStreetVehicle (vehicleId: VehicleId) (updater: StreetVehicle -> StreetVehicle) (world: World) =
        { world with
            Street =
                { world.Street with
                    Vehicles =
                        world.Street.Vehicles
                        |> Map.change vehicleId (Option.map updater) } }

    let private neighborhoodForPlace placeId world =
        world.Neighborhoods
        |> Map.toSeq
        |> Seq.tryFind (fun (_, neighborhood) ->
            Set.contains placeId neighborhood.Businesses
            || world.HousingUnits
               |> Map.toSeq
               |> Seq.exists (fun (_, unit) ->
                   unit.Neighborhood = neighborhood.Id
                   && world.Households
                      |> Map.toSeq
                      |> Seq.exists (fun (_, household) -> household.Home = placeId && Set.contains household.Id unit.Occupants)))
        |> Option.map fst

    let private addItemToActor (actorId: ActorId) (item: StreetItem) world =
        updateActor actorId (fun actor -> { actor with Inventory = Map.add item.Id item actor.Inventory }) world

    let private invalidateTransportCaches world =
        { world with
            Transport =
                { world.Transport with
                    TravelTimeReliability = Map.empty
                    SegmentCongestion =
                        world.Map.RoadSegments
                        |> List.map (fun segment ->
                            segment.Id,
                            world.Transport.SegmentCongestion
                            |> Map.tryFind segment.Id
                            |> Option.defaultValue 0.0)
                        |> Map.ofList }
            Runtime =
                { world.Runtime with
                    RouteCache = Map.empty
                    TravelTimeCache = Map.empty
                    CacheVersion = world.Runtime.CacheVersion + 1 } }

    let applyEvent event world =
        match event with
        | ActorMoved(eventId, actorId, _, toLocation) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Location = toLocation
                    CurrentActivity = ActorMoving })
            |> addStreetEventId eventId
        | ActorEnteredVehicle(eventId, actorId, vehicleId) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Location = ActorInsideVehicle vehicleId
                    CurrentVehicle = Some vehicleId
                    CurrentActivity = ActorDriving })
            |> addStreetEventId eventId
        | ActorExitedVehicle(eventId, actorId, vehicleId, location) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Location = location
                    CurrentVehicle = None
                    CurrentActivity = ActorIdle })
            |> updateStreetVehicle vehicleId (fun vehicle ->
                { vehicle with
                    Controller = if vehicle.Controller = Some actorId then None else vehicle.Controller
                    Location = location })
            |> addStreetEventId eventId
        | ActorGainedVehicleControl(eventId, actorId, vehicleId) ->
            world
            |> updateActor actorId (fun actor -> { actor with CurrentVehicle = Some vehicleId })
            |> updateStreetVehicle vehicleId (fun vehicle -> { vehicle with Controller = Some actorId })
            |> addStreetEventId eventId
        | ActorLostVehicleControl(eventId, actorId, vehicleId) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    CurrentVehicle = if actor.CurrentVehicle = Some vehicleId then None else actor.CurrentVehicle })
            |> updateStreetVehicle vehicleId (fun vehicle ->
                { vehicle with
                    Controller = if vehicle.Controller = Some actorId then None else vehicle.Controller })
            |> addStreetEventId eventId
        | VehicleMoved(eventId, vehicleId, _, toLocation) ->
            world
            |> updateStreetVehicle vehicleId (fun vehicle -> { vehicle with Location = toLocation })
            |> addStreetEventId eventId
        | ActorEnteredBuilding(eventId, actorId, buildingId) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Location = ActorInsideBuilding buildingId
                    CurrentActivity = ActorInsideBuildingActivity })
            |> addStreetEventId eventId
        | ActorExitedBuilding(eventId, actorId, _, location) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Location = location
                    CurrentActivity = ActorIdle })
            |> addStreetEventId eventId
        | VehicleDamaged(eventId, vehicleId, severity) ->
            world
            |> updateStreetVehicle vehicleId (fun vehicle -> { vehicle with Damage = clamp01 (vehicle.Damage + severity) })
            |> addStreetEventId eventId
        | ItemPurchased(eventId, actorId, placeId, item, price) ->
            let world =
                world
                |> addItemToActor actorId item
                |> updateActor actorId (fun actor ->
                    { actor with
                        CurrentActivity = ActorShopping
                        Reputation = clamp01 (actor.Reputation + 0.002) })

            let sims =
                match Map.tryFind actorId world.Street.Actors |> Option.bind _.PersonId with
                | Some simId ->
                    world.Sims
                    |> Map.change simId (Option.map (fun sim -> { sim with Wallet = sim.Wallet - price }))
                | None -> world.Sims

            let places =
                world.Map.Places
                |> Map.change placeId (Option.map (fun place ->
                    { place with
                        Economy =
                            place.Economy
                            |> Option.map (fun economy ->
                                { economy with
                                    Cash = economy.Cash + price
                                    Inventory =
                                        match item.Good with
                                        | Some good ->
                                            economy.Inventory
                                            |> Map.change good (fun existing -> Some(max 0.0 ((existing |> Option.defaultValue 0.0) - 1.0)))
                                        | None -> economy.Inventory }) }))

            { world with Sims = sims; Map = { world.Map with Places = places } }
            |> addStreetEventId eventId
        | ItemTakenWithoutPayment(eventId, actorId, placeId, item) ->
            let places =
                world.Map.Places
                |> Map.change placeId (Option.map (fun place ->
                    { place with
                        Economy =
                            place.Economy
                            |> Option.map (fun economy ->
                                { economy with
                                    Inventory =
                                        match item.Good with
                                        | Some good ->
                                            economy.Inventory
                                            |> Map.change good (fun existing -> Some(max 0.0 ((existing |> Option.defaultValue 0.0) - 1.0)))
                                        | None -> economy.Inventory }) }))

            { world with Map = { world.Map with Places = places } }
            |> addItemToActor actorId item
            |> addStreetEventId eventId
        | UnauthorizedEntryAttempted(eventId, _, _)
        | UnauthorizedEntrySucceeded(eventId, _, _)
        | UnauthorizedEntryFailed(eventId, _, _)
        | UnauthorizedVehicleAccessAttempted(eventId, _, _)
        | UnauthorizedVehicleAccessSucceeded(eventId, _, _)
        | UnauthorizedVehicleAccessFailed(eventId, _, _)
        | VehicleAlarmTriggered(eventId, _)
        | ObjectUsed(eventId, _, _)
        | PersonInteractionOccurred(eventId, _, _)
        | ConflictStarted(eventId, _, _)
        | ConflictEscalated(eventId, _, _)
        | ConflictResolved(eventId, _, _)
        | TheftReported(eventId, _, _)
        | TrespassReported(eventId, _, _)
        | CrimeReported(eventId, _, _, _)
        | WitnessObservedEvent(eventId, _, _, _, _)
        | PoliceArrived(eventId, _)
        | EmergencyServiceCalled(eventId, _, _)
        | EmergencyServiceArrived(eventId, _)
        | VehicleCollisionOccurred(eventId, _, _) ->
            addStreetEventId eventId world
        | PropertyDamaged(eventId, _, target, severity) ->
            let world =
                match target with
                | BuildingRef buildingId ->
                    { world with
                        Street =
                            { world.Street with
                                Buildings =
                                    world.Street.Buildings
                                    |> Map.change buildingId (Option.map (fun building -> { building with Condition = clamp01 (building.Condition - severity) })) } }
                | VehicleRef vehicleId ->
                    updateStreetVehicle vehicleId (fun vehicle -> { vehicle with Damage = clamp01 (vehicle.Damage + severity) }) world
                | _ -> world

            addStreetEventId eventId world
        | PoliceDispatched(eventId, dispatch) ->
            { world with
                Street =
                    { world.Street with
                        Dispatches = Map.add dispatch.Id dispatch world.Street.Dispatches } }
            |> addStreetEventId eventId
        | ActorDetained(eventId, actorId, _institutionId) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    LegalStatus = DetainedStatus
                    CurrentActivity = ActorIdle })
            |> addStreetEventId eventId
        | ActorFined(eventId, actorId, amount) ->
            let world =
                updateActor actorId (fun actor -> { actor with LegalStatus = FinedStatus }) world

            let sims =
                match Map.tryFind actorId world.Street.Actors |> Option.bind _.PersonId with
                | Some simId -> world.Sims |> Map.change simId (Option.map (fun sim -> { sim with Wallet = max 0m (sim.Wallet - amount) }))
                | None -> world.Sims

            { world with Sims = sims } |> addStreetEventId eventId
        | ActorReleased(eventId, actorId) ->
            world
            |> updateActor actorId (fun actor -> { actor with LegalStatus = NoLegalConcern })
            |> addStreetEventId eventId
        | ActorInjured(eventId, actorId) ->
            world
            |> updateActor actorId (fun actor -> { actor with Health = Injured })
            |> addStreetEventId eventId
        | BusinessInterrupted(eventId, placeId, severity) ->
            let places =
                world.Map.Places
                |> Map.change placeId (Option.map (fun place ->
                    { place with
                        Economy =
                            place.Economy
                            |> Option.map (fun economy -> { economy with Cash = max 0m (economy.Cash - decimal (severity * 25.0)) }) }))

            { world with Map = { world.Map with Places = places } }
            |> addStreetEventId eventId
        | NeighborhoodSafetyChanged(eventId, neighborhoodId, delta) ->
            { world with
                Neighborhoods =
                    world.Neighborhoods
                    |> Map.change neighborhoodId (Option.map (fun neighborhood -> { neighborhood with Safety = clamp01 (neighborhood.Safety + delta) }))
                City =
                    { world.City with
                        Indicators = { world.City.Indicators with Crime = clamp01 (world.City.Indicators.Crime + max 0.0 (-delta)) } } }
            |> addStreetEventId eventId
        | InstitutionalTrustChanged(eventId, institutionId, neighborhoodId, delta) ->
            let institutions =
                match institutionId with
                | Some id -> world.Institutions |> Map.change id (Option.map (fun institution -> { institution with Trust = clamp01 (institution.Trust + delta) }))
                | None -> world.Institutions

            let neighborhoods =
                match neighborhoodId with
                | Some id -> world.Neighborhoods |> Map.change id (Option.map (fun neighborhood -> { neighborhood with InstitutionalTrust = clamp01 (neighborhood.InstitutionalTrust + delta) }))
                | None -> world.Neighborhoods

            { world with Institutions = institutions; Neighborhoods = neighborhoods }
            |> addStreetEventId eventId
        | ReputationChanged(eventId, actorId, delta) ->
            world
            |> updateActor actorId (fun actor -> { actor with Reputation = clamp01 (actor.Reputation + delta) })
            |> addStreetEventId eventId
        | WantedLevelChanged(eventId, actorId, _, toLevel)
        | HeatIncreased(eventId, actorId, _, toLevel)
        | HeatDecreased(eventId, actorId, _, toLevel) ->
            world
            |> updateActor actorId (fun actor ->
                { actor with
                    Heat = toLevel
                    LegalStatus = if heatRank toLevel > 0 then UnderInvestigation else actor.LegalStatus })
            |> addStreetEventId eventId
        | BuildingConstructed(_, _, _, parcelId, building, cost) ->
            { world with
                City =
                    { world.City with
                        Parcels =
                            world.City.Parcels
                            |> Map.change parcelId (Option.map (fun parcel ->
                                { parcel with
                                    Building = Some building
                                    Desirability = clamp01 (parcel.Desirability + 0.02)
                                    LandValue = clamp01 (parcel.LandValue + 0.03) })) } }
            |> chargeCityBudget cost
        | BuildingDestroyed(_, _, _, parcelId, _, cost) ->
            { world with
                City =
                    { world.City with
                        Parcels =
                            world.City.Parcels
                            |> Map.change parcelId (Option.map (fun parcel ->
                                { parcel with
                                    Building = None
                                    Desirability = clamp01 (parcel.Desirability - 0.10)
                                    LandValue = clamp01 (parcel.LandValue - 0.08)
                                    FireRisk = clamp01 (parcel.FireRisk + 0.04) })) } }
            |> chargeCityBudget cost
        | BuildingModified(_, _, BuildingId parcelId, modification) ->
            { world with
                City =
                    { world.City with
                        Parcels =
                            world.City.Parcels
                            |> Map.change parcelId (Option.map (fun parcel ->
                                { parcel with
                                    Building =
                                        parcel.Building
                                        |> Option.map (fun building ->
                                            match modification with
                                            | ChangeUse useKind -> { building with Use = useKind }
                                            | AddUnits count -> { building with Capacity = building.Capacity + count }
                                            | RemoveUnits count -> { building with Capacity = max 0 (building.Capacity - count) }
                                            | Renovate
                                            | Repair
                                            | RetrofitForAccessibility
                                            | EnergyUpgrade -> { building with Status = Occupied }
                                            | Condemn -> { building with Status = Abandoned }
                                            | Reopen -> { building with Status = Occupied }
                                            | CloseTemporarily -> { building with Status = Vacant }
                                            | AddParking _
                                            | RemoveParking _
                                            | AddCommercialSpace _
                                            | ConvertToMixedUse -> building) })) } }
        | HouseholdsDisplaced(_, _, _, households) ->
            { world with
                Households =
                    (world.Households, households)
                    ||> List.fold (fun households householdId ->
                        households
                        |> Map.change householdId (Option.map (fun household ->
                            { household with
                                HousingStatus = Shelter
                                Stability = clamp01 (household.Stability - 0.30)
                                ConflictLevel = clamp01 (household.ConflictLevel + 0.20)
                                SharedGoals = ("find replacement housing" :: household.SharedGoals) |> List.distinct }))) }
        | ParcelZoned(_, _, parcelId, zone, density) ->
            { world with
                City =
                    { world.City with
                        Parcels =
                            world.City.Parcels
                            |> Map.change parcelId (Option.map (fun parcel ->
                                { parcel with
                                    Zone = zone
                                    Density = density
                                    LandValue = clamp01 (parcel.LandValue + 0.01) })) } }
        | ParcelRezoned(_, _, parcelId, _, toZone) ->
            { world with
                City =
                    { world.City with
                        Parcels =
                            world.City.Parcels
                            |> Map.change parcelId (Option.map (fun parcel ->
                                { parcel with
                                    Zone = toZone
                                    LandValue = clamp01 (parcel.LandValue + 0.02) })) } }
        | RoadBuilt(_, _, segment, lanes, cost) ->
            { world with
                Map =
                    { world.Map with
                        RoadSegments =
                            (world.Map.RoadSegments @ [ segment ])
                            |> List.sortBy _.Id }
                Transport =
                    { world.Transport with
                        Lanes =
                            (world.Transport.Lanes, lanes)
                            ||> List.fold (fun lanes lane -> Map.add lane.Id lane lanes)
                        SegmentCongestion = Map.add segment.Id 0.0 world.Transport.SegmentCongestion } }
            |> chargeCityBudget cost
            |> invalidateTransportCaches
        | RoadDestroyed(_, _, roadSegmentId, _) ->
            let removedLaneIds =
                world.Map.RoadSegments
                |> List.tryFind (fun segment -> segment.Id = roadSegmentId)
                |> Option.map (fun segment -> segment.LaneIds |> Set.ofList)
                |> Option.defaultValue Set.empty

            let intersections =
                world.Transport.Intersections
                |> Map.map (fun _ intersection ->
                    { intersection with
                        IncomingLanes = Set.difference intersection.IncomingLanes removedLaneIds
                        OutgoingLanes = Set.difference intersection.OutgoingLanes removedLaneIds
                        PermittedMovements =
                            intersection.PermittedMovements
                            |> Map.filter (fun laneId _ -> not (Set.contains laneId removedLaneIds)) })

            { world with
                Map =
                    { world.Map with
                        RoadSegments = world.Map.RoadSegments |> List.filter (fun segment -> segment.Id <> roadSegmentId) }
                Transport =
                    { world.Transport with
                        Lanes = world.Transport.Lanes |> Map.filter (fun laneId _ -> not (Set.contains laneId removedLaneIds))
                        Intersections = intersections
                        SegmentCongestion = world.Transport.SegmentCongestion |> Map.remove roadSegmentId } }
            |> invalidateTransportCaches
        | BillDue(_, householdId, amount) ->
            { world with
                Households =
                    world.Households
                    |> Map.change householdId (Option.map (fun household -> { household with BillsDue = household.BillsDue + amount })) }
        | BillPaid(_, householdId, amount) ->
            { world with
                Households =
                    world.Households
                    |> Map.change householdId (Option.map (fun household ->
                        { household with
                            Funds = household.Funds - amount
                            BillsDue = max 0m (household.BillsDue - amount)
                            Stability = clamp01 (household.Stability + 0.04) })) }
        | BillMissed(_, householdId, amount) ->
            { world with
                Households =
                    world.Households
                    |> Map.change householdId (Option.map (fun household ->
                        { household with
                            BillsDue = household.BillsDue + amount * 0.05m
                            Stability = clamp01 (household.Stability - 0.12)
                            ConflictLevel = clamp01 (household.ConflictLevel + 0.07) })) }
        | HouseholdBudgetChanged(_, _, _) -> world
        | RentIncreased(_, householdId, _, newRent) ->
            { world with
                Households =
                    world.Households
                    |> Map.change householdId (Option.map (fun household ->
                        { household with
                            RentMonthly = Some newRent
                            MonthlyExpenses = household.MonthlyExpenses + newRent
                            Stability = clamp01 (household.Stability - 0.08) })) }
        | SchoolDayCompleted(_, simId, _) ->
            { world with
                Sims =
                    world.Sims
                    |> Map.change simId (Option.map (fun sim ->
                        { sim with
                            Needs =
                                sim.Needs
                                |> Map.change Learning (Option.map (fun need -> { need with Value = clamp01 (need.Value + 0.05) })) })) }
        | ChildMissedSchool(_, simId, _) ->
            { world with
                Sims =
                    world.Sims
                    |> Map.change simId (Option.map (fun sim ->
                        { sim with
                            Happiness = clamp01 (sim.Happiness - 0.05)
                            Needs =
                                sim.Needs
                                |> Map.change Learning (Option.map (fun need -> { need with Value = clamp01 (need.Value - 0.08) })) })) }
        | TransportEventOccurred(_, ArrivedLate(simId, _, delay)) ->
            let stress = min 0.18 (float delay / 120.0)

            let sims =
                world.Sims
                |> Map.change simId (Option.map (fun sim ->
                    { sim with
                        Happiness = clamp01 (sim.Happiness - stress)
                        Needs =
                            sim.Needs
                            |> Map.change Comfort (Option.map (fun need -> { need with Value = clamp01 (need.Value - stress * 0.60) }))
                            |> Map.change Purpose (Option.map (fun need -> { need with Value = clamp01 (need.Value - stress * 0.45) })) }))

            let households =
                match Map.tryFind simId world.Sims with
                | Some sim ->
                    world.Households
                    |> Map.change sim.Household (Option.map (fun household ->
                        { household with
                            Stability = clamp01 (household.Stability - stress * 0.30)
                            ConflictLevel = clamp01 (household.ConflictLevel + stress * 0.25)
                            TransportationAccess = clamp01 (household.TransportationAccess - stress * 0.20) }))
                | None -> world.Households

            { world with Sims = sims; Households = households }
        | TransportEventOccurred(_, ParkingFailed tripId) ->
            match Map.tryFind tripId world.Transport.Trips |> Option.bind _.HouseholdId with
            | Some householdId ->
                { world with
                    Households =
                        world.Households
                        |> Map.change householdId (Option.map (fun household ->
                            { household with
                                Stability = clamp01 (household.Stability - 0.03)
                                ConflictLevel = clamp01 (household.ConflictLevel + 0.04)
                                TransportationAccess = clamp01 (household.TransportationAccess - 0.02) })) }
            | None -> world
        | TransportEventOccurred(_, TransitTrustChanged(householdId, delta)) ->
            match householdId with
            | Some householdId ->
                { world with
                    Households =
                        world.Households
                        |> Map.change householdId (Option.map (fun household ->
                            { household with TransportationAccess = clamp01 (household.TransportationAccess + delta) })) }
            | None -> world
        | _ -> world

    let applyEventAndRemember event world =
        let world = applyEvent event world

        match memoryFromEvent world world.Memories.Count event with
        | Some memory ->
            { world with Memories = Map.add memory.Id memory world.Memories }
            |> appendMemoryToPeople memory
        | None -> world

    let applyEvents events world =
        (world, events) ||> List.fold (fun world event -> applyEventAndRemember event world)

    let private decayMemories world =
        let memories =
            world.Memories
            |> Map.toSeq
            |> Seq.choose (fun (memoryId, memory) ->
                let aged =
                    { memory with EmotionalWeight = clamp01 (memory.EmotionalWeight - memory.DecayPerDay / 24.0) }

                if aged.EmotionalWeight <= 0.05 && aged.Salience = Trivial then
                    None
                else
                    Some(memoryId, aged))
            |> Map.ofSeq

        { world with Memories = memories }

    let private lowerHeat =
        function
        | CitywideAlert -> HighHeat
        | HighHeat -> ModerateHeat
        | ModerateHeat -> LowHeat
        | LowHeat -> NoHeat
        | NoHeat -> NoHeat

    let private decayStreetHeat world =
        if world.Meta.Tick % 2 <> 0 then
            world
        else
            let actors =
                world.Street.Actors
                |> Map.map (fun _ actor ->
                    let next = lowerHeat actor.Heat
                    { actor with
                        Heat = next
                        LegalStatus = if next = NoHeat then NoLegalConcern else actor.LegalStatus })

            { world with Street = { world.Street with Actors = actors } }

    let tick world =
        let indexes = rebuildIndexes world
        let runtime = rebuildRuntimeIndexes world
        let world =
            { world with
                Meta = { world.Meta with Indexes = indexes }
                Runtime = runtime }

        let systemEvents = generateSystemEvents world
        let world = { world with Transport = { world.Transport with RecentEvents = [] } }
        let world = (world, systemEvents) ||> List.fold (fun world event -> applyEventAndRemember event world)
        let intents = generateIntents world
        let resolved = resolveIntents intents
        let intentEvents = resolved |> List.mapi (eventsFromIntent world) |> List.concat
        let events = systemEvents @ intentEvents

        let world =
            (world, intentEvents)
            ||> List.fold (fun world event -> applyEventAndRemember event world)

        let indexes = rebuildIndexes world
        let runtime = rebuildRuntimeIndexes world
        let diagnostics =
            { world.PerformanceDiagnostics with
                TripsProcessed = world.Transport.Trips.Count
                IntentsGenerated = resolved.Length
                EventsEmitted = events.Length
                EventLogCompactions = if events.Length + world.Meta.EventLog.Length > 500 then world.PerformanceDiagnostics.EventLogCompactions + 1 else world.PerformanceDiagnostics.EventLogCompactions
                PartitionWorkloads = runtime.TripsByPartition |> Map.map (fun _ trips -> trips.Length) }

        { world with
            Meta =
                { world.Meta with
                    Tick = world.Meta.Tick + 1
                    EventLog = (events @ world.Meta.EventLog) |> List.truncate 500
                    Decisions = (resolved |> List.map _.Decision) @ world.Meta.Decisions |> List.truncate 500
                    Indexes = indexes }
            Runtime = runtime
            PerformanceDiagnostics = diagnostics }
        |> decayMemories
        |> decayStreetHeat
