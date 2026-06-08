namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module TrafficSystem =
    let private movementKey (movementId: MovementId) =
        match movementId with
        | MovementId id -> id

    let private isActiveMovement (movement: MovementState) =
        match movement.Status with
        | MovementStatus.Completed
        | MovementStatus.Canceled
        | MovementStatus.Failed -> false
        | _ -> true

    let private vehicleIdOfKind =
        function
        | MovingEntityKind.Vehicle vehicleId
        | MovingEntityKind.EmergencyResponder vehicleId
        | MovingEntityKind.FreightVehicle vehicleId
        | MovingEntityKind.ServiceVehicle vehicleId -> Some vehicleId
        | _ -> None

    let private isPedestrian =
        function
        | MovingEntityKind.Pedestrian _ -> true
        | _ -> false

    let private currentLeg (movement: MovementState) =
        movement.Route.Legs |> List.tryItem movement.CurrentLegIndex

    let private currentSegmentId (movement: MovementState) =
        movement |> currentLeg |> Option.bind _.SegmentId

    let private currentIntersectionId (movement: MovementState) =
        match movement.Status, currentLeg movement with
        | Simulation.Domain.MovementStatus.WaitingAtIntersection, Some leg -> leg.ToRoadNode
        | Simulation.Domain.MovementStatus.Blocked, Some leg -> leg.ToRoadNode
        | _ -> None

    let private isQueued (movement: MovementState) =
        match movement.Status with
        | Simulation.Domain.MovementStatus.Queued
        | Simulation.Domain.MovementStatus.WaitingAtIntersection
        | Simulation.Domain.MovementStatus.Blocked -> true
        | _ -> false

    let private currentLane (transport: TransportState) (movement: MovementState) =
        match vehicleIdOfKind movement.Kind with
        | Some vehicleId ->
            transport.Vehicles
            |> Map.tryFind vehicleId
            |> Option.bind _.CurrentLane
            |> Option.orElseWith (fun () -> currentLeg movement |> Option.bind (fun leg -> leg.LaneIds |> List.tryHead))
        | None ->
            currentLeg movement
            |> Option.bind (fun leg -> leg.LaneIds |> List.tryHead)

    let private modeAllowed (mode: TravelMode) (lane: Lane) =
        Set.contains mode lane.AllowedModes

    let private vehicleDriver (transport: TransportState) (movement: MovementState) =
        vehicleIdOfKind movement.Kind
        |> Option.bind (fun vehicleId -> transport.Vehicles |> Map.tryFind vehicleId)
        |> Option.map _.Driver
        |> Option.defaultValue
            { Aggressiveness = 0.45
              Patience = 0.55
              Familiarity = 0.55
              RiskTolerance = 0.45
              LawCompliance = 0.65
              StressLevel = 0.20
              Urgency = 0.35
              HighwayAversion = 0.30
              TollAversion = 0.30
              RerouteTendency = 0.35
              WalkingToleranceMeters = 1500.0
              TransitTolerance = 0.50 }

    let private nextManeuver (transport: TransportState) (movement: MovementState) =
        vehicleIdOfKind movement.Kind
        |> Option.bind (fun vehicleId -> transport.Vehicles |> Map.tryFind vehicleId)
        |> Option.bind _.NextRequiredMovement
        |> Option.orElseWith (fun () ->
            movement.Route.Legs
            |> List.tryItem (movement.CurrentLegIndex + 1)
            |> Option.map (fun _ -> MoveThrough))

    let private distanceToManeuver (movement: MovementState) =
        currentLeg movement
        |> Option.map (fun leg -> max 0.0 (leg.DistanceMeters - movement.DistanceOnLegMeters))
        |> Option.defaultValue 0.0

    let private lookaheadMeters (roadClass: RoadClass) speedKph (driver: DriverProfile) =
        let classBase =
            match roadClass with
            | Freeway
            | Highway -> 1400.0
            | Arterial
            | FreightCorridor
            | TransitCorridor -> 850.0
            | Collector
            | IndustrialRoad -> 600.0
            | _ -> 350.0

        let speedFactor = 1.0 + clamp01 (speedKph / 90.0) * 0.65
        let cautionFactor = 0.75 + driver.Patience * 0.45 + driver.Familiarity * 0.20 - driver.Aggressiveness * 0.25
        classBase * speedFactor * max 0.55 cautionFactor

    let private lanePreference mode desiredMovement (lane: Lane) =
        let movementFit =
            match desiredMovement with
            | Some movement when Set.contains movement lane.PermittedMovements -> 30.0
            | Some _ -> -60.0
            | None -> 0.0

        let modeFit =
            match mode, lane.LaneType with
            | Bus, BusOnly -> 35.0
            | Bike, BikeOnly
            | Bike, ProtectedBike -> 40.0
            | PrivateCar, BusOnly
            | PrivateCar, BikeOnly
            | PrivateCar, ProtectedBike -> -100.0
            | FreightTruck, Loading -> 8.0
            | EmergencyVehicle, Shoulder -> 12.0
            | _, General
            | _, Through
            | _, ThroughLeft
            | _, ThroughRight -> 10.0
            | _ -> 0.0

        movementFit + modeFit - lane.CurrentDensity * 20.0 - (if lane.Blocked then 100.0 else 0.0)

    let private bestLaneOnSegment mode desiredMovement segmentLaneIds (transport: TransportState) =
        segmentLaneIds
        |> List.choose (fun laneId -> transport.Lanes |> Map.tryFind laneId)
        |> List.filter (fun (lane: Lane) -> modeAllowed mode lane)
        |> List.sortByDescending (fun (lane: Lane) -> lanePreference mode desiredMovement lane, lane.Id)
        |> List.tryHead

    let private laneChangeReason mode (fromLane: Lane) (toLane: Lane) desiredMovement =
        if fromLane.Blocked then AvoidBlockedLane
        elif mode = Bus && toLane.LaneType = BusOnly then EnterBusLane
        elif not (modeAllowed mode fromLane) then LeaveRestrictedLane
        else
            match desiredMovement with
            | Some MoveLeft
            | Some MoveRight -> PrepareForTurn
            | Some MergeLeft
            | Some MergeRight -> MergeRequired
            | _ when toLane.CurrentDensity + 0.10 < fromLane.CurrentDensity -> AvoidCongestion
            | _ -> PrepareForExit

    let evaluateLaneChangeProposals (world: World) (transport: TransportState) =
        let segmentById = world.Map.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList

        transport.Movements
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter isActiveMovement
        |> Seq.choose (fun movement ->
            match currentSegmentId movement, currentLane transport movement with
            | Some segmentId, Some fromLaneId ->
                match Map.tryFind segmentId segmentById, Map.tryFind fromLaneId transport.Lanes with
                | Some segment, Some (fromLane: Lane) ->
                    let mode = movement.Route.Mode
                    let driver = vehicleDriver transport movement
                    let desiredMovement = nextManeuver transport movement
                    let distance = distanceToManeuver movement
                    let lookahead = lookaheadMeters segment.RoadClass movement.CurrentSpeedKph driver
                    let currentWorks =
                        modeAllowed mode fromLane
                        && not fromLane.Blocked
                        && (desiredMovement |> Option.forall (fun movement -> Set.contains movement fromLane.PermittedMovements))
                    let shouldPrepare = distance <= lookahead || not currentWorks || fromLane.CurrentDensity > 0.72

                    if not shouldPrepare then
                        None
                    else
                        bestLaneOnSegment mode desiredMovement segment.LaneIds transport
                        |> Option.filter (fun (toLane: Lane) -> toLane.Id <> fromLaneId)
                        |> Option.map (fun (toLane: Lane) ->
                            let urgency = clamp01 ((lookahead - distance) / max 1.0 lookahead + driver.Urgency * 0.25 + driver.Aggressiveness * 0.15)
                            let safety = clamp01 (driver.LawCompliance * 0.35 + driver.Patience * 0.25 + (1.0 - toLane.CurrentDensity) * 0.40)
                            { MovementId = movement.Id
                              FromLane = fromLaneId
                              ToLane = toLane.Id
                              Reason = laneChangeReason mode fromLane toLane desiredMovement
                              Urgency = urgency
                              SafetyScore = safety
                              GapAcceptance = clamp01 (driver.RiskTolerance * 0.45 + driver.Aggressiveness * 0.45 + (1.0 - driver.Patience) * 0.10)
                              DriverProfile = driver })
                | _ -> None
            | _ -> None)
        |> Seq.sortBy (fun proposal -> -proposal.Urgency, -proposal.SafetyScore, movementKey proposal.MovementId)
        |> Seq.toList

    let private resolveLaneChangeProposals (transport: TransportState) (proposals: LaneChangeProposal list) =
        let laneVehicleCounts =
            transport.LaneOccupancies
            |> Map.map (fun _ occupancy -> occupancy.VehicleCount + occupancy.CyclistCount)

        let folder (acceptedTargets: Map<LaneId, int>, usedMovements: Set<MovementId>, results: (LaneChangeProposal * bool) list) (proposal: LaneChangeProposal) =
            if Set.contains proposal.MovementId usedMovements then
                acceptedTargets, usedMovements, (proposal, false) :: results
            else
                let targetLane = transport.Lanes |> Map.tryFind proposal.ToLane
                let movement = transport.Movements |> Map.tryFind proposal.MovementId
                let valid =
                    match targetLane, movement with
                    | Some lane, Some movement ->
                        let count = acceptedTargets |> Map.tryFind proposal.ToLane |> Option.defaultValue 0
                        let capacity = max 1 (int (Math.Ceiling(lane.CapacityPerHour / 60.0 * 5.0)))
                        modeAllowed movement.Route.Mode lane
                        && not lane.Blocked
                        && count < capacity
                        && (proposal.SafetyScore >= 0.25 || proposal.GapAcceptance >= 0.65)
                    | _ -> false

                if valid then
                    let acceptedTargets = acceptedTargets |> Map.change proposal.ToLane (fun count -> Some((count |> Option.defaultValue 0) + 1))
                    let usedMovements = Set.add proposal.MovementId usedMovements
                    acceptedTargets, usedMovements, (proposal, true) :: results
                else
                    acceptedTargets, usedMovements, (proposal, false) :: results

        let _, _, results = proposals |> List.fold folder (laneVehicleCounts, Set.empty, ([]: (LaneChangeProposal * bool) list))
        results |> List.rev

    let private applyLaneChanges (results: (LaneChangeProposal * bool) list) (transport: TransportState) =
        let laneForMovement =
            results
            |> List.choose (fun (proposal, accepted) -> if accepted then Some(proposal.MovementId, proposal.ToLane) else None)
            |> Map.ofList

        let vehicles =
            transport.Vehicles
            |> Map.map (fun _ vehicle ->
                transport.Movements
                |> Map.toSeq
                |> Seq.tryFind (fun (_, movement) -> vehicleIdOfKind movement.Kind = Some vehicle.Id)
                |> Option.bind (fun (movementId, _) -> laneForMovement |> Map.tryFind movementId)
                |> Option.map (fun laneId -> { vehicle with CurrentLane = Some laneId; DelayMinutes = max 0 (vehicle.DelayMinutes - 1) })
                |> Option.defaultValue vehicle)

        let events =
            results
            |> List.collect (fun (proposal, accepted) ->
                [ LaneChangeRequested(proposal.MovementId, proposal.FromLane, proposal.ToLane, proposal.Reason)
                  match transport.Movements |> Map.tryFind proposal.MovementId |> Option.bind (fun movement -> vehicleIdOfKind movement.Kind) with
                  | Some vehicleId when accepted -> LaneChanged(vehicleId, proposal.FromLane, proposal.ToLane)
                  | Some vehicleId -> LaneChangeFailed(vehicleId, proposal.FromLane, proposal.ToLane)
                  | None -> () ])

        { transport with Vehicles = vehicles }, events

    let private deriveSignalPhaseStates seconds (transport: TransportState) =
        transport.Intersections
        |> Map.toSeq
        |> Seq.choose (fun (nodeId, intersection) ->
            match intersection.Control, intersection.SignalPhases with
            | Signalized _, phases
            | AdaptiveSignal, phases
            | TransitPrioritySignal, phases when not phases.IsEmpty ->
                let cycle = phases |> List.sumBy _.DurationSeconds |> max 1
                let previous =
                    transport.SignalPhaseStates
                    |> Map.tryFind nodeId
                    |> Option.defaultValue
                        { IntersectionId = nodeId
                          CurrentPhaseIndex = 0
                          SecondsRemaining = phases.Head.DurationSeconds
                          CycleSeconds = cycle }
                let mutable index = previous.CurrentPhaseIndex
                let mutable remaining = previous.SecondsRemaining - seconds
                let mutable changed = false
                while remaining <= 0 do
                    index <- (index + 1) % phases.Length
                    remaining <- remaining + phases[index].DurationSeconds
                    changed <- true

                Some(nodeId,
                     { IntersectionId = nodeId
                       CurrentPhaseIndex = index
                       SecondsRemaining = remaining
                       CycleSeconds = cycle },
                     changed)
            | _ -> None)
        |> Seq.toList

    let private currentAllowedMovements (transport: TransportState) nodeId =
        match transport.Intersections |> Map.tryFind nodeId, transport.SignalPhaseStates |> Map.tryFind nodeId with
        | Some intersection, Some state when not intersection.SignalPhases.IsEmpty ->
            intersection.SignalPhases
            |> List.tryItem state.CurrentPhaseIndex
            |> Option.map _.Movements
            |> Option.defaultValue Set.empty
        | Some intersection, _ ->
            match intersection.Control with
            | Signalized _
            | AdaptiveSignal
            | TransitPrioritySignal -> Set.empty
            | _ -> [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList
        | _ -> [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList

    let private serviceIntersectionQueues (transport: TransportState) =
        let queued =
            transport.Movements
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter isQueued
            |> Seq.choose (fun movement ->
                currentIntersectionId movement
                |> Option.map (fun nodeId -> nodeId, movement))
            |> Seq.groupBy fst

        queued
        |> Seq.collect (fun (nodeId, entries) ->
            let movements = entries |> Seq.map snd
            let capacity =
                transport.Intersections
                |> Map.tryFind nodeId
                |> Option.map (fun intersection -> max 1 intersection.CapacityPerMinute)
                |> Option.defaultValue 1
            let allowed = currentAllowedMovements transport nodeId

            movements
            |> Seq.sortBy (fun movement -> movement.CurrentLegIndex, movementKey movement.Id)
            |> Seq.filter (fun movement -> nextManeuver transport movement |> Option.forall (fun maneuver -> Set.contains maneuver allowed))
            |> Seq.truncate capacity
            |> Seq.map (fun movement -> IntersectionMovementServed(movement.Id, nodeId)))
        |> Seq.toList

    let private movementLaneGroups transport movements =
        movements
        |> List.choose (fun movement -> currentLane transport movement |> Option.map (fun laneId -> laneId, movement))
        |> List.groupBy fst
        |> Map.ofList

    let private deriveLaneOccupancies (transport: TransportState) =
        let movements =
            transport.Movements
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter isActiveMovement
            |> Seq.toList

        let grouped = movementLaneGroups transport movements

        transport.Lanes
        |> Map.map (fun laneId lane ->
            let entries = grouped |> Map.tryFind laneId |> Option.defaultValue []
            let laneMovements = entries |> List.map snd |> List.sortBy (fun movement -> movementKey movement.Id)
            let vehicleCount = laneMovements |> List.sumBy (fun movement -> if (vehicleIdOfKind movement.Kind).IsSome then 1 else 0)
            let pedestrianCount = laneMovements |> List.sumBy (fun movement -> if isPedestrian movement.Kind then 1 else 0)
            let cyclistCount =
                laneMovements
                |> List.sumBy (fun movement ->
                    match movement.Kind with
                    | MovingEntityKind.Cyclist _ -> 1
                    | _ -> 0)
            let queueLength = laneMovements |> List.sumBy (fun movement -> if isQueued movement then 1 else 0)
            let capacity = max 1.0 (lane.CapacityPerHour / 60.0 * 5.0)
            let density = clamp01 (float (vehicleCount + cyclistCount) / capacity)
            let averageSpeed =
                match laneMovements with
                | [] -> lane.CurrentSpeedKph
                | active -> active |> List.averageBy _.CurrentSpeedKph
            let spillback = queueLength >= int (Math.Ceiling capacity) || lane.Blocked

            { LaneId = laneId
              MovementIds = laneMovements |> List.map _.Id
              VehicleCount = vehicleCount
              PedestrianCount = pedestrianCount
              CyclistCount = cyclistCount
              Density = density
              QueueLength = queueLength
              AverageSpeedKph = averageSpeed
              IsBlocked = lane.Blocked
              Spillback = spillback })

    let private deriveModeSegmentOccupancy modePredicate transport =
        transport.Movements
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun movement -> isActiveMovement movement && modePredicate movement)
        |> Seq.choose (fun movement -> currentSegmentId movement |> Option.map (fun segmentId -> segmentId, movement.Id))
        |> Seq.groupBy fst
        |> Seq.map (fun (segmentId, ids) -> segmentId, ids |> Seq.map snd |> Seq.sortBy movementKey |> Seq.toList)
        |> Map.ofSeq

    let private groupedCount key movements =
        movements
        |> Seq.choose key
        |> Seq.countBy id
        |> Map.ofSeq

    let updateTransportState (world: World) (transport: TransportState) =
        let movements =
            transport.Movements
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter isActiveMovement
            |> Seq.toList

        let signalStatesWithChanges = deriveSignalPhaseStates 60 transport
        let signalPhaseStates =
            signalStatesWithChanges
            |> List.map (fun (nodeId, state, _) -> nodeId, state)
            |> Map.ofList

        let phaseEvents =
            signalStatesWithChanges
            |> List.choose (fun (nodeId, state, changed) -> if changed then Some(SignalPhaseChanged(nodeId, state.CurrentPhaseIndex)) else None)

        let previousLaneOccupancies = transport.LaneOccupancies
        let transport = { transport with SignalPhaseStates = signalPhaseStates }
        let initialOccupancies = deriveLaneOccupancies transport
        let transport = { transport with LaneOccupancies = initialOccupancies }
        let proposals = evaluateLaneChangeProposals world transport
        let proposalResults = resolveLaneChangeProposals transport proposals
        let transport, laneEvents = applyLaneChanges proposalResults transport
        let laneOccupancies = deriveLaneOccupancies transport

        let vehicleMovements =
            movements
            |> List.filter (fun movement -> (vehicleIdOfKind movement.Kind).IsSome)

        let pedestrianMovements =
            movements
            |> List.filter (fun movement -> isPedestrian movement.Kind)

        let vehicleCountBySegment =
            vehicleMovements
            |> groupedCount currentSegmentId

        let pedestrianCountBySegment =
            pedestrianMovements
            |> groupedCount currentSegmentId

        let queuedBySegment =
            vehicleMovements
            |> Seq.filter isQueued
            |> groupedCount currentSegmentId

        let waitingByIntersection =
            vehicleMovements
            |> groupedCount currentIntersectionId

        let speedsBySegment =
            vehicleMovements
            |> Seq.choose (fun movement -> currentSegmentId movement |> Option.map (fun segmentId -> segmentId, movement.CurrentSpeedKph))
            |> Seq.groupBy fst
            |> Seq.map (fun (segmentId, speeds) -> segmentId, speeds |> Seq.averageBy snd)
            |> Map.ofSeq

        let congestionBySegment =
            world.Map.RoadSegments
            |> List.map (fun segment ->
                let vehicles = vehicleCountBySegment |> Map.tryFind segment.Id |> Option.defaultValue 0 |> float
                let pedestrians = pedestrianCountBySegment |> Map.tryFind segment.Id |> Option.defaultValue 0 |> float
                let queued = queuedBySegment |> Map.tryFind segment.Id |> Option.defaultValue 0 |> float
                let capacity = max 1.0 (float segment.CapacityPerMinute * 15.0)
                let closurePenalty =
                    if segment.UnderConstruction || not segment.CurrentIncidents.IsEmpty then 0.35 else 0.0
                let occupancyPressure = (vehicles + pedestrians * 0.15) / capacity
                let queuePressure = queued / max 1.0 (float segment.CapacityPerMinute)
                segment.Id, clamp01 (occupancyPressure + queuePressure + closurePenalty))
            |> Map.ofList

        let segmentById =
            world.Map.RoadSegments
            |> List.map (fun segment -> segment.Id, segment)
            |> Map.ofList

        let intersectionEvents = serviceIntersectionQueues { transport with LaneOccupancies = laneOccupancies }

        let spillbackEvents =
            laneOccupancies
            |> Map.toSeq
            |> Seq.collect (fun (laneId, occupancy) ->
                let previous = previousLaneOccupancies |> Map.tryFind laneId
                [ if occupancy.Spillback && previous |> Option.forall (fun previous -> not previous.Spillback) then
                      QueueSpillbackStarted laneId
                  if not occupancy.Spillback && previous |> Option.exists _.Spillback then
                      QueueSpillbackEnded laneId
                  if occupancy.IsBlocked then
                      DownstreamBlocked laneId ])
            |> Seq.toList

        let queueEvents =
            laneOccupancies
            |> Map.toSeq
            |> Seq.collect (fun (laneId, occupancy) ->
                occupancy.MovementIds
                |> List.choose (fun movementId ->
                    transport.Movements
                    |> Map.tryFind movementId
                    |> Option.filter isQueued
                    |> Option.map (fun _ -> VehicleQueued(movementId, laneId))))
            |> Seq.toList

        let lanes =
            transport.Lanes
            |> Map.map (fun _ lane ->
                let segment = Map.tryFind lane.SegmentId segmentById
                let congestion = congestionBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0.0
                let segmentVehicleCount = vehicleCountBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0
                let laneOccupancy = laneOccupancies |> Map.tryFind lane.Id
                let queueLength = laneOccupancy |> Option.map _.QueueLength |> Option.defaultValue (queuedBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0)
                let laneCount = segment |> Option.map (fun segment -> max 1 segment.LaneIds.Length) |> Option.defaultValue 1
                let capacity = max 1.0 (lane.CapacityPerHour / 60.0 * 15.0)
                let density = laneOccupancy |> Option.map _.Density |> Option.defaultValue (clamp01 ((float segmentVehicleCount / float laneCount) / capacity))
                let baseSpeed = segment |> Option.map (fun roadSegment -> roadSegment.SpeedKph) |> Option.defaultValue lane.CurrentSpeedKph
                let isClosed = segment |> Option.exists (fun segment -> segment.UnderConstruction || not segment.CurrentIncidents.IsEmpty)
                let speed = baseSpeed * (1.0 - min 0.80 (congestion * 0.65))

                { lane with
                    CurrentDensity = max density congestion
                    CurrentSpeedKph = if isClosed then 0.0 else max 3.0 speed
                    QueueLength = queueLength
                    Blocked = isClosed || congestion > 0.98 || laneOccupancy |> Option.exists _.Spillback })

        let laneOccupancies =
            deriveLaneOccupancies { transport with Lanes = lanes }

        let pedestrianSegmentOccupancy =
            deriveModeSegmentOccupancy (fun movement -> isPedestrian movement.Kind) transport

        let bikeSegmentOccupancy =
            deriveModeSegmentOccupancy (fun movement ->
                match movement.Kind with
                | MovingEntityKind.Cyclist _ -> true
                | _ -> false) transport

        let averageSegmentSpeed =
            match speedsBySegment |> Map.toSeq |> Seq.map snd |> Seq.toList with
            | [] -> 0.0
            | speeds -> speeds |> List.average

        let averageCongestion =
            congestionBySegment
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.append [ 0.0 ]
            |> Seq.average

        { transport with
            Lanes = lanes
            LaneOccupancies = laneOccupancies
            PedestrianSegmentOccupancy = pedestrianSegmentOccupancy
            BikeSegmentOccupancy = bikeSegmentOccupancy
            SegmentCongestion = congestionBySegment
            RecentEvents = (transport.RecentEvents @ phaseEvents @ laneEvents @ intersectionEvents @ spillbackEvents @ queueEvents) |> List.truncate 200
            Metrics =
                { transport.Metrics with
                    AverageCongestion = averageCongestion
                    ActiveVehicleCount = vehicleMovements.Length
                    ActivePedestrianCount = pedestrianMovements.Length
                    AverageSegmentSpeedKph = averageSegmentSpeed
                    QueuedVehicleCount = queuedBySegment |> Map.toSeq |> Seq.sumBy snd
                    IntersectionWaitingCount = waitingByIntersection |> Map.toSeq |> Seq.sumBy snd } }
