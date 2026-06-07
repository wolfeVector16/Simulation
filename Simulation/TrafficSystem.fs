namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module TrafficSystem =
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

        let lanes =
            transport.Lanes
            |> Map.map (fun _ lane ->
                let segment = Map.tryFind lane.SegmentId segmentById
                let congestion = congestionBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0.0
                let segmentVehicleCount = vehicleCountBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0
                let queueLength = queuedBySegment |> Map.tryFind lane.SegmentId |> Option.defaultValue 0
                let laneCount = segment |> Option.map (fun segment -> max 1 segment.LaneIds.Length) |> Option.defaultValue 1
                let capacity = max 1.0 (lane.CapacityPerHour / 60.0 * 15.0)
                let density = clamp01 ((float segmentVehicleCount / float laneCount) / capacity)
                let baseSpeed = segment |> Option.map (fun roadSegment -> roadSegment.SpeedKph) |> Option.defaultValue lane.CurrentSpeedKph
                let isClosed = segment |> Option.exists (fun segment -> segment.UnderConstruction || not segment.CurrentIncidents.IsEmpty)
                let speed = baseSpeed * (1.0 - min 0.80 (congestion * 0.65))

                { lane with
                    CurrentDensity = max density congestion
                    CurrentSpeedKph = if isClosed then 0.0 else max 3.0 speed
                    QueueLength = queueLength
                    Blocked = isClosed || congestion > 0.98 })

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
            SegmentCongestion = congestionBySegment
            Metrics =
                { transport.Metrics with
                    AverageCongestion = averageCongestion
                    ActiveVehicleCount = vehicleMovements.Length
                    ActivePedestrianCount = pedestrianMovements.Length
                    AverageSegmentSpeedKph = averageSegmentSpeed
                    QueuedVehicleCount = queuedBySegment |> Map.toSeq |> Seq.sumBy snd
                    IntersectionWaitingCount = waitingByIntersection |> Map.toSeq |> Seq.sumBy snd } }
