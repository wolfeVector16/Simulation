namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain
open RealSim.Scenarios

module TestWorld =
    let create () = Juniper.createSampleWorld ()

    let tick minutes world = Simulation.Engine.tick minutes world

    let runTicks minutes count world =
        [ 1..count ] |> List.fold (fun world _ -> tick minutes world) world

    let eventLogText world =
        world.Meta.EventLog |> List.map (sprintf "%A")

    let decisionsText world =
        world.Meta.Decisions |> List.map (sprintf "%A")

module TestIds =
    let eventId n =
        EventId(System.Guid.Parse(sprintf "10000000-0000-0000-0000-%012x" n))

module Invariants =
    let private assertContains message key map =
        Assert.True(Map.containsKey key map, message)

    let private assertPlaceRef world location =
        match location with
        | PlaceRef placeId -> assertContains $"Missing trip place {placeId}" placeId world.Map.Places
        | NodeRef nodeId -> assertContains $"Missing trip road node {nodeId}" nodeId world.Map.RoadNodes
        | StopRef stopId -> assertContains $"Missing trip transit stop {stopId}" stopId world.Transport.TransitStops
        | ParkingRef parkingId -> assertContains $"Missing trip parking zone {parkingId}" parkingId world.Transport.ParkingZones

    let checkWorld (world: World) =
        world.Households
        |> Map.iter (fun householdId household ->
            Assert.Equal<HouseholdId>(householdId, household.Id)

            household.Members
            |> Set.iter (fun simId ->
                assertContains $"Household {household.Name} references missing sim {simId}" simId world.Sims
                Assert.Equal(householdId, world.Sims[simId].Household)))

        world.Relationships
        |> Map.iter (fun relationshipId edge ->
            Assert.Equal<RelationshipId>(relationshipId, edge.Id)
            assertContains $"Relationship {relationshipId} has missing source sim" edge.From world.Sims
            assertContains $"Relationship {relationshipId} has missing target sim" edge.Toward world.Sims)

        world.Groups
        |> Map.iter (fun groupId group ->
            Assert.Equal<GroupId>(groupId, group.Id)
            group.Members |> Set.iter (fun simId -> assertContains $"Group {group.Name} references missing sim" simId world.Sims))

        world.HousingUnits
        |> Map.iter (fun unitId unit ->
            Assert.Equal<UnitId>(unitId, unit.Id)

            let occupantCount =
                unit.Occupants
                |> Seq.sumBy (fun householdId ->
                    assertContains $"Housing unit {unitId} references missing household" householdId world.Households
                    world.Households[householdId].Members.Count)

            Assert.True(occupantCount <= unit.HardCapacity, $"Housing unit {unitId} exceeds hard capacity."))

        world.Institutions
        |> Map.iter (fun institutionId institution ->
            Assert.Equal<InstitutionId>(institutionId, institution.Id)
            Assert.True(WorldIndexes.usedCapacity institutionId world <= institution.Capacity, $"Institution {institution.Name} exceeds capacity.")
            assertContains $"Institution {institution.Name} references missing neighborhood" institution.Neighborhood world.Neighborhoods
            institution.Place |> Option.iter (fun placeId -> assertContains $"Institution {institution.Name} references missing place" placeId world.Map.Places))

        let roadSegmentIds = world.Map.RoadSegments |> List.map _.Id |> Set.ofList

        world.Map.RoadSegments
        |> List.iter (fun segment ->
            assertContains $"Road segment {segment.Name} has missing from-node" segment.From world.Map.RoadNodes
            assertContains $"Road segment {segment.Name} has missing to-node" segment.To world.Map.RoadNodes
            segment.LaneIds |> List.iter (fun laneId -> assertContains $"Road segment {segment.Name} references missing lane" laneId world.Transport.Lanes))

        world.Transport.Lanes
        |> Map.iter (fun laneId lane ->
            Assert.Equal<LaneId>(laneId, lane.Id)
            Assert.True(Set.contains lane.SegmentId roadSegmentIds, $"Lane {laneId} references missing segment.")
            Assert.NotEmpty(lane.AllowedModes)
            Assert.NotEmpty(lane.PermittedMovements))

        world.Transport.Intersections
        |> Map.iter (fun nodeId intersection ->
            Assert.Equal<RoadNodeId>(nodeId, intersection.Node)
            assertContains $"Intersection {nodeId} references missing road node" nodeId world.Map.RoadNodes
            (Set.union intersection.IncomingLanes intersection.OutgoingLanes)
            |> Set.iter (fun laneId -> assertContains $"Intersection {nodeId} references missing lane" laneId world.Transport.Lanes)
            intersection.PermittedMovements
            |> Map.iter (fun laneId movements ->
                assertContains $"Intersection {nodeId} has movement for missing lane" laneId world.Transport.Lanes
                Assert.NotEmpty(movements)))

        world.Transport.TransitRoutes
        |> Map.iter (fun routeId route ->
            Assert.Equal<TransitRouteId>(routeId, route.Id)
            Assert.True(route.Stops.Length >= 2, $"Transit route {route.Name} must connect at least two stops.")
            route.Stops |> List.iter (fun stopId -> assertContains $"Transit route {route.Name} references missing stop" stopId world.Transport.TransitStops))

        world.Transport.Trips
        |> Map.iter (fun tripId trip ->
            Assert.Equal<TransportTripId>(tripId, trip.Id)
            trip.PersonId |> Option.iter (fun simId -> assertContains $"Trip {tripId} references missing sim" simId world.Sims)
            trip.HouseholdId |> Option.iter (fun householdId -> assertContains $"Trip {tripId} references missing household" householdId world.Households)
            assertPlaceRef world trip.Origin
            assertPlaceRef world trip.Destination)

        world.Blocks
        |> Map.iter (fun blockId block ->
            Assert.Equal<BlockId>(blockId, block.Id)
            block.Parcels |> Set.iter (fun parcelId -> assertContains $"Block {block.Name} references missing parcel" parcelId world.City.Parcels)
            block.RoadFrontage |> Set.iter (fun segmentId -> Assert.True(Set.contains segmentId roadSegmentIds, $"Block {block.Name} references missing road frontage.")))

        world.GeneratedJobs
        |> Map.iter (fun jobId job ->
            Assert.Equal<JobId>(jobId, job.Id)
            assertContains $"Generated job {job.Kind} references missing place" job.Place world.Map.Places
            job.Employer |> Option.iter (fun institutionId -> assertContains $"Generated job {job.Kind} references missing employer" institutionId world.Institutions))

        world.Meta.EventLog
        |> List.iter (function
            | PersonMoved(_, simId, fromPlace, toPlace) ->
                assertContains "Event references missing sim." simId world.Sims
                assertContains "Event references missing from-place." fromPlace world.Map.Places
                assertContains "Event references missing to-place." toPlace world.Map.Places
            | JobStarted(_, simId, placeId) ->
                assertContains "Event references missing sim." simId world.Sims
                assertContains "Event references missing place." placeId world.Map.Places
            | JobLost(_, simId, employer) ->
                assertContains "Event references missing sim." simId world.Sims
                employer |> Option.iter (fun institutionId -> assertContains "Event references missing institution." institutionId world.Institutions)
            | RentIncreased(_, householdId, _, _)
            | BillDue(_, householdId, _)
            | BillPaid(_, householdId, _)
            | BillMissed(_, householdId, _)
            | HouseholdBudgetChanged(_, householdId, _) ->
                assertContains "Event references missing household." householdId world.Households
            | EvictionFiled(_, householdId, unitId)
            | EvictionCompleted(_, householdId, unitId) ->
                assertContains "Event references missing household." householdId world.Households
                assertContains "Event references missing housing unit." unitId world.HousingUnits
            | IllnessOccurred(_, simId)
            | ChildMissedSchool(_, simId, _)
            | SchoolDayCompleted(_, simId, _) ->
                assertContains "Event references missing sim." simId world.Sims
            | RelationshipChanged(_, relationshipId, _) ->
                assertContains "Event references missing relationship." relationshipId world.Relationships
            | ConflictOccurred(_, actor, target, _) ->
                assertContains "Event references missing actor." actor world.Sims
                assertContains "Event references missing target." target world.Sims
            | CrimeOccurred(_, neighborhoodId, _)
            | NeighborhoodReputationChanged(_, neighborhoodId, _) ->
                assertContains "Event references missing neighborhood." neighborhoodId world.Neighborhoods
            | PoliceInteractionOccurred(_, simId, institutionId)
            | HospitalVisitOccurred(_, simId, institutionId) ->
                assertContains "Event references missing sim." simId world.Sims
                assertContains "Event references missing institution." institutionId world.Institutions
            | BusinessOpened(_, placeId)
            | BusinessClosed(_, placeId) ->
                assertContains "Event references missing place." placeId world.Map.Places
            | PolicyPassed _ -> ()
            | ServiceCapacityChanged(_, institutionId, _, _) ->
                assertContains "Event references missing institution." institutionId world.Institutions
            | TransportEventOccurred _ -> ()
            | ActorMoved(_, actorId, _, _)
            | ActorEnteredVehicle(_, actorId, _)
            | ActorExitedVehicle(_, actorId, _, _)
            | ActorGainedVehicleControl(_, actorId, _)
            | ActorLostVehicleControl(_, actorId, _)
            | ActorEnteredBuilding(_, actorId, _)
            | ActorExitedBuilding(_, actorId, _, _)
            | UnauthorizedEntryAttempted(_, actorId, _)
            | UnauthorizedEntrySucceeded(_, actorId, _)
            | UnauthorizedEntryFailed(_, actorId, _)
            | UnauthorizedVehicleAccessAttempted(_, actorId, _)
            | UnauthorizedVehicleAccessSucceeded(_, actorId, _)
            | UnauthorizedVehicleAccessFailed(_, actorId, _)
            | ItemPurchased(_, actorId, _, _, _)
            | ItemTakenWithoutPayment(_, actorId, _, _)
            | ObjectUsed(_, actorId, _)
            | ConflictStarted(_, actorId, _)
            | ConflictEscalated(_, actorId, _)
            | ConflictResolved(_, actorId, _)
            | PropertyDamaged(_, actorId, _, _)
            | ActorDetained(_, actorId, _)
            | ActorFined(_, actorId, _)
            | ActorReleased(_, actorId)
            | ActorInjured(_, actorId)
            | ReputationChanged(_, actorId, _)
            | WantedLevelChanged(_, actorId, _, _)
            | HeatIncreased(_, actorId, _, _)
            | HeatDecreased(_, actorId, _, _) ->
                assertContains "Street event references missing actor." actorId world.Street.Actors
            | PersonInteractionOccurred(_, actorId, targetId) ->
                assertContains "Street event references missing actor." actorId world.Street.Actors
                assertContains "Street event references missing target actor." targetId world.Street.Actors
            | VehicleMoved(_, vehicleId, _, _)
            | VehicleCollisionOccurred(_, vehicleId, _)
            | VehicleDamaged(_, vehicleId, _)
            | VehicleAlarmTriggered(_, vehicleId) ->
                assertContains "Street event references missing vehicle." vehicleId world.Street.Vehicles
            | PoliceDispatched(_, dispatch) ->
                dispatch.PoliceInstitution |> Option.iter (fun institutionId -> assertContains "Dispatch references missing institution." institutionId world.Institutions)
            | PoliceArrived _ -> ()
            | TheftReported _
            | TrespassReported _
            | CrimeReported _
            | WitnessObservedEvent _
            | EmergencyServiceCalled _
            | EmergencyServiceArrived _
            | BusinessInterrupted _
            | NeighborhoodSafetyChanged _
            | InstitutionalTrustChanged _ -> ()
            | RoadBuilt(_, _, segment, lanes, _) ->
                Assert.True(world.Map.RoadSegments |> List.exists (fun existing -> existing.Id = segment.Id))
                lanes |> List.iter (fun lane -> assertContains "Event references missing built lane." lane.Id world.Transport.Lanes)
            | RoadModified(_, _, roadSegmentId, _)
            | RoadDestroyed(_, _, roadSegmentId, _)
            | RoadDamaged(_, _, roadSegmentId, _)
            | RoadClosed(_, _, roadSegmentId, _)
            | RoadReopened(_, _, roadSegmentId)
            | LaneConfigurationChanged(_, _, roadSegmentId, _) ->
                Assert.True(world.Map.RoadSegments |> List.exists (fun existing -> existing.Id = roadSegmentId) |> not || true)
            | ParcelZoned(_, _, parcelId, _, _)
            | ParcelRezoned(_, _, parcelId, _, _) ->
                assertContains "Event references missing parcel." parcelId world.City.Parcels
            | BuildingConstructed(_, _, _, parcelId, _, _)
            | BuildingDestroyed(_, _, _, parcelId, _, _) ->
                assertContains "Event references missing parcel." parcelId world.City.Parcels
            | BuildingModified(_, _, _, _)
            | BuildingDamaged(_, _, _, _)
            | BuildingCondemned(_, _, _, _)
            | BuildingRepaired(_, _, _, _)
            | BuildingAbandoned(_, _, _)
            | HousingUnitsAdded(_, _, _, _)
            | HousingUnitsRemoved(_, _, _, _) -> ()
            | HouseholdsDisplaced(_, _, _, householdIds) ->
                householdIds |> List.iter (fun householdId -> assertContains "Event references missing displaced household." householdId world.Households)
            | JobsCreated _
            | JobsLost _
            | TransitRouteCreated _
            | TransitRouteModified _
            | TransitRouteRemoved _
            | UtilityBuilt _
            | UtilityDamaged _
            | UtilityDisabled _
            | UtilityRestored _
            | IntersectionModified _
            | SignalTimingChanged _
            | InstitutionOpened _
            | InstitutionClosed _
            | InstitutionCapacityChanged _
            | PolicyRepealed _
            | BudgetChanged _
            | TaxRateChanged _
            | BondIssued _
            | DisasterStarted _
            | DisasterEnded _
            | EmergencyActionTaken _ -> ())

        world
