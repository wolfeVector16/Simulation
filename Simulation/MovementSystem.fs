namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module MovementSystem =
    let private active (status: MovementStatus) =
        match status with
        | MovementStatus.Planned
        | MovementStatus.Waiting
        | MovementStatus.InProgress
        | MovementStatus.Queued
        | MovementStatus.WaitingAtIntersection
        | MovementStatus.Blocked
        | MovementStatus.Delayed -> true
        | MovementStatus.Completed
        | MovementStatus.Canceled
        | MovementStatus.Failed -> false

    let private legStartDistance (route: TransportRoute) legIndex =
        route.Legs
        |> List.take (min legIndex route.Legs.Length)
        |> List.sumBy _.DistanceMeters

    let private legIndexAtDistance (route: TransportRoute) distance =
        let rec loop index remaining legs =
            match legs with
            | [] -> max 0 (route.Legs.Length - 1), 0.0
            | leg :: _ when remaining <= leg.DistanceMeters || index = route.Legs.Length - 1 ->
                index, min leg.DistanceMeters (max 0.0 remaining)
            | leg :: rest ->
                loop (index + 1) (remaining - leg.DistanceMeters) rest

        loop 0 (max 0.0 distance) route.Legs

    let private positionAtRouteDistance (route: TransportRoute) distance =
        let progress =
            if route.TotalDistanceMeters <= 0.0 then 0.0
            else clamp01 (distance / route.TotalDistanceMeters)

        TransportRoute.interpolate progress route

    let private headingBetween (a: Coordinates) (b: Coordinates) =
        Math.Atan2(b.Y - a.Y, b.X - a.X)

    let private currentSegmentId (movement: MovementState) =
        movement.Route.Legs
        |> List.tryItem movement.CurrentLegIndex
        |> Option.bind _.SegmentId

    let private freeFlowSpeedFor (movement: MovementState) =
        if movement.CurrentSpeedKph > 0.0 then movement.CurrentSpeedKph
        else
            match movement.Route.Mode with
            | Walk -> 4.8
            | Bike -> 15.0
            | Bus -> 28.0
            | Tram
            | Metro
            | RegionalRail -> 35.0
            | EmergencyVehicle -> 55.0
            | FreightTruck
            | DeliveryVehicle -> 32.0
            | ServiceVehicle -> 30.0
            | _ -> 35.0

    let private speedFor (world: World) (movement: MovementState) =
        let freeFlow = freeFlowSpeedFor movement

        match currentSegmentId movement with
        | Some segmentId when movement.Route.Mode <> Walk ->
            let congestion =
                world.Transport.SegmentCongestion
                |> Map.tryFind segmentId
                |> Option.defaultValue 0.0

            freeFlow * (1.0 - min 0.75 (congestion * 0.55))
        | _ -> freeFlow

    let private routeDistance (movement: MovementState) =
        legStartDistance movement.Route movement.CurrentLegIndex + movement.DistanceOnLegMeters

    let private blockedByInvalidGeometry (movement: MovementState) =
        movement.Route.Legs.IsEmpty
        || movement.Route.Geometry.Polyline.Length < 2
        || movement.Route.TotalDistanceMeters <= 0.0

    let private advanceMovement minutes world (movement: MovementState) : MovementState * TransportEvent list =
        if not (active movement.Status) then
            movement, []
        elif blockedByInvalidGeometry movement then
            { movement with
                PreviousPosition = Some movement.CurrentPosition
                Status = MovementStatus.Failed
                CurrentSpeedKph = 0.0
                DelaySeconds = movement.DelaySeconds + minutes * 60 },
            [ MovementFailed(movement.Id, movement.TripId) ]
        else
            let previousPosition = movement.CurrentPosition
            let currentSpeed = speedFor world movement
            let meters = currentSpeed * 1000.0 / 60.0 * float minutes
            let targetDistance = min movement.Route.TotalDistanceMeters (routeDistance movement + max 0.0 meters)
            let nextPosition = positionAtRouteDistance movement.Route targetDistance

            match nextPosition with
            | None ->
                { movement with
                    PreviousPosition = Some previousPosition
                    Status = MovementStatus.Blocked
                    CurrentSpeedKph = 0.0
                    DelaySeconds = movement.DelaySeconds + minutes * 60 },
                [ MovementBlocked(movement.Id, movement.TripId) ]
            | Some position ->
                let legIndex, distanceOnLeg = legIndexAtDistance movement.Route targetDistance
                let progress = if movement.Route.TotalDistanceMeters <= 0.0 then 0.0 else clamp01 (targetDistance / movement.Route.TotalDistanceMeters)
                let completed = targetDistance >= movement.Route.TotalDistanceMeters
                let heading =
                    if position = previousPosition then movement.HeadingRadians
                    else Some(headingBetween previousPosition position)

                { movement with
                    CurrentLegIndex = legIndex
                    DistanceOnLegMeters = distanceOnLeg
                    Progress = progress
                    CurrentPosition = position
                    PreviousPosition = Some previousPosition
                    HeadingRadians = heading
                    CurrentSpeedKph = if completed then 0.0 else currentSpeed
                    Status = if completed then MovementStatus.Completed else MovementStatus.InProgress },
                [ if completed then MovementCompleted(movement.Id, movement.TripId) ]

    let private locationPlace =
        function
        | PlaceRef placeId -> Some placeId
        | _ -> None

    let private completeTrip (world: World) (movement: MovementState) (transport: TransportState) =
        match Map.tryFind movement.TripId transport.Trips with
        | None -> world, transport, []
        | Some trip ->
            let transport =
                { transport with
                    Trips = Map.add trip.Id { trip with Status = TripStatus.Completed } transport.Trips
                    Vehicles =
                        transport.Vehicles
                        |> Map.map (fun _ vehicle ->
                            if vehicle.Trip = trip.Id then
                                { vehicle with
                                    PreviousPosition = Some vehicle.CurrentPosition
                                    CurrentPosition = CompletedTripPosition
                                    CurrentSpeedKph = 0.0
                                    CurrentRouteIndex = None
                                    Status = VehicleCompleted
                                    DelayMinutes = 0 }
                            else
                                vehicle) }

            let world =
                match trip.PersonId, locationPlace trip.Destination with
                | Some simId, Some destination ->
                    { world with
                        Sims =
                            world.Sims
                            |> Map.change simId (Option.map (fun sim -> { sim with Location = AtPlace destination })) }
                | _ -> world

            let delay =
                match trip.DeadlineMinute with
                | Some deadline -> max 0 (normalizeMinute world.MinuteOfDay - deadline)
                | None -> 0

            let events =
                [ TripCompleted trip.Id
                  if delay > trip.ToleranceForDelayMinutes then
                      match trip.PersonId with
                      | Some simId -> ArrivedLate(simId, trip.Purpose, delay)
                      | None -> () ]

            world, transport, events

    let tick minutes (world: World) =
        let advanced =
            world.Transport.Movements
            |> Map.toSeq
            |> Seq.map (fun (movementId, (movement: MovementState)) ->
                let movement, events = advanceMovement minutes world movement
                movementId, movement, events)
            |> Seq.toList

        let movements =
            advanced
            |> List.map (fun (movementId, movement, _) -> movementId, movement)
            |> Map.ofList

        let movementEvents = advanced |> List.collect (fun (_, _, events) -> events)
        let mutable world = world
        let mutable transport = { world.Transport with Movements = movements }
        let mutable completionEvents: TransportEvent list = []

        for _, movement, events in advanced do
            if events |> List.exists (function MovementCompleted _ -> true | _ -> false) then
                let nextWorld, nextTransport, events = completeTrip world movement transport
                world <- nextWorld
                transport <- nextTransport
                completionEvents <- completionEvents @ events

        { world with
            Transport =
                { transport with
                    RecentEvents = world.Transport.RecentEvents @ movementEvents @ completionEvents } }
