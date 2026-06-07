namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module TransportRouting =
    type RoadRoute =
        { TotalMinutes: float
          TotalDistanceMeters: float
          AccessMeters: float
          Origin: RouteEndpoint
          Destination: RouteEndpoint
          Legs: RouteLeg list
          Geometry: RouteGeometry
          Segments: RoadSegment list
          NodePath: RoadNodeId list
          SegmentTravelMinutes: int list
          IntersectionDelayMinutes: int list }

    let private minutesAtSpeed speedKph meters =
        if speedKph <= 0.0 then
            Int32.MaxValue
        else
            max 1 (int (Math.Ceiling((meters / 1000.0) / speedKph * 60.0)))

    let congestionFor world (segment: RoadSegment) =
        world.Transport.SegmentCongestion
        |> Map.tryFind segment.Id
        |> Option.defaultValue 0.0

    let segmentEffectiveSpeedKph world (segment: RoadSegment) =
        let congestion = congestionFor world segment
        segment.SpeedKph * (1.0 - min 0.75 (congestion * 0.55)) * segment.SurfaceCondition * (1.0 - segment.WeatherImpact)

    let segmentTravelMinutes world (segment: RoadSegment) =
        let length = MapGraph.segmentLength world.Map segment
        let incidentPenalty =
            if segment.CurrentIncidents.IsEmpty && not segment.UnderConstruction then 1.0
            else 1.0 + max 0.25 (float segment.CurrentIncidents.Count * 0.35)

        minutesAtSpeed (segmentEffectiveSpeedKph world segment) length |> float |> (*) incidentPenalty

    let private nodePosition world nodeId =
        world.Map.RoadNodes
        |> Map.tryFind nodeId
        |> Option.map _.Position

    let private vectorIntoNode world nodeId (segment: RoadSegment) =
        match nodePosition world nodeId with
        | None -> None
        | Some node ->
            let other =
                if segment.To = nodeId then
                    nodePosition world segment.From
                elif segment.From = nodeId then
                    nodePosition world segment.To
                else
                    None

            other |> Option.map (fun other -> node.X - other.X, node.Y - other.Y)

    let private vectorOutOfNode world nodeId (segment: RoadSegment) =
        match nodePosition world nodeId with
        | None -> None
        | Some node ->
            let other =
                if segment.From = nodeId then
                    nodePosition world segment.To
                elif segment.To = nodeId && segment.IsTwoWay then
                    nodePosition world segment.From
                else
                    None

            other |> Option.map (fun other -> other.X - node.X, other.Y - node.Y)

    let classifyIntersectionMovement world nodeId (previousSegment: RoadSegment option) (nextSegment: RoadSegment) =
        match previousSegment with
        | None -> Straight
        | Some previous when previous.Id = nextSegment.Id -> UTurnMovement
        | Some previous ->
            match vectorIntoNode world nodeId previous, vectorOutOfNode world nodeId nextSegment with
            | Some (ax, ay), Some (bx, by) ->
                let dot = ax * bx + ay * by
                let cross = ax * by - ay * bx
                let angle = Math.Atan2(cross, dot)
                let absAngle = abs angle

                if absAngle < Math.PI / 7.0 then Straight
                elif absAngle > Math.PI * 0.78 then UTurnMovement
                elif cross > 0.0 then LeftTurnMovement
                else RightTurnMovement
            | _ ->
                if previous.Name = nextSegment.Name then Straight else UnknownMovement

    let intersectionDelayMinutes world mode nodeId previousSegment nextSegment =
        match world.Transport.Intersections |> Map.tryFind nodeId with
        | None -> 0
        | Some intersection ->
            let movement = classifyIntersectionMovement world nodeId previousSegment nextSegment
            let controlSeconds =
                match intersection.Control with
                | Uncontrolled -> 12.0
                | Yield -> 35.0
                | StopSign -> 75.0
                | AllWayStop -> 110.0
                | Signalized _ -> 150.0
                | AdaptiveSignal -> 120.0
                | Roundabout -> 80.0
                | RampMeter -> 210.0
                | RailroadCrossing -> 240.0
                | PedestrianCrossing -> 90.0
                | TransitPrioritySignal -> 70.0

            let movementSeconds =
                match movement with
                | Straight -> 0.0
                | RightTurnMovement -> 18.0
                | LeftTurnMovement -> 70.0
                | UTurnMovement -> 120.0
                | MergeMovement
                | DivergeMovement -> 35.0
                | UnknownMovement -> 55.0

            let modeSeconds =
                match mode with
                | Bus when intersection.SignalPhases |> List.exists (fun phase -> phase.Kind = TransitPriorityPhase) -> -55.0
                | EmergencyVehicle when intersection.SignalPhases |> List.exists (fun phase -> phase.Kind = EmergencyPreemptionPhase) -> -80.0
                | Bike -> max 0.0 ((1.0 - intersection.BikeCrossingQuality) * 45.0)
                | Walk -> max 0.0 ((1.0 - intersection.CrosswalkQuality) * 55.0)
                | _ -> 0.0

            let congestionSeconds =
                let incomingQueue =
                    intersection.IncomingLanes
                    |> Seq.choose (fun laneId -> world.Transport.Lanes |> Map.tryFind laneId)
                    |> Seq.map (fun lane -> float lane.QueueLength)
                    |> Seq.append [ 0.0 ]
                    |> Seq.average

                incomingQueue * 3.0 + intersection.QueueSpillbackRisk * 45.0 + intersection.MergeDifficulty * 25.0

            int (Math.Ceiling(max 0.0 (controlSeconds + movementSeconds + modeSeconds + congestionSeconds) / 60.0))

    let private placeEndpoint world placeId =
        world.Map.Places
        |> Map.tryFind placeId
        |> Option.map (fun place ->
            { PlaceId = Some placeId
              RoadNodeId = None
              Position = place.Position
              Name = Some place.Name })

    let private nodeEndpoint world nodeId =
        world.Map.RoadNodes
        |> Map.tryFind nodeId
        |> Option.map (fun node ->
            { PlaceId = None
              RoadNodeId = Some nodeId
              Position = node.Position
              Name = None })

    let private geometry distance points =
        { Polyline = points; DistanceMeters = distance }

    let private leg mode fromEndpoint toEndpoint distance minutes segmentId laneIds fromNode toNode segmentMinutes intersectionDelay =
        { Mode = mode
          From = fromEndpoint
          To = toEndpoint
          Geometry = geometry distance [ fromEndpoint.Position; toEndpoint.Position ]
          DistanceMeters = distance
          ExpectedMinutes = minutes
          SegmentId = segmentId
          LaneIds = laneIds
          FromRoadNode = fromNode
          ToRoadNode = toNode
          SegmentTravelMinutes = segmentMinutes
          IntersectionDelayMinutes = intersectionDelay }

    let private appendPoint point points =
        match points with
        | last :: _ when last = point -> points
        | _ -> point :: points

    let private routeGeometry legs =
        let points =
            ([], legs)
            ||> List.fold (fun points leg ->
                points
                |> appendPoint leg.From.Position
                |> appendPoint leg.To.Position)
            |> List.rev

        geometry (legs |> List.sumBy _.DistanceMeters) points

    let roadRoute world mode origin destination =
        match MapGraph.resolveRoadAccess world.Map origin, MapGraph.resolveRoadAccess world.Map destination with
        | Some originAccess, Some destinationAccess ->
            let edgeCost node previousSegment (edge: MapGraph.RoadEdge) =
                segmentTravelMinutes world edge.Segment
                + float (intersectionDelayMinutes world mode node previousSegment edge.Segment)

            match placeEndpoint world origin, placeEndpoint world destination with
            | Some originEndpoint, Some destinationEndpoint ->
                MapGraph.shortestRoadPathWithCost world.Map originAccess.Node destinationAccess.Node edgeCost
                |> Option.bind (fun path ->
                    match nodeEndpoint world originAccess.Node, nodeEndpoint world destinationAccess.Node with
                    | Some originNodeEndpoint, Some destinationNodeEndpoint ->
                        let accessMinutes = minutesAtSpeed 5.0 originAccess.AccessMeters
                        let egressMinutes = minutesAtSpeed 5.0 destinationAccess.AccessMeters

                        let accessLegs =
                            [ if originAccess.AccessMeters > 0.0 then
                                  leg mode originEndpoint originNodeEndpoint originAccess.AccessMeters accessMinutes None [] None (Some originAccess.Node) accessMinutes 0 ]

                        let roadLegs =
                            path.Steps
                            |> List.mapi (fun index step ->
                                let previousSegment =
                                    if index = 0 then None
                                    else path.Steps |> List.tryItem (index - 1)
                                    |> Option.map _.Segment

                                let segmentMinutes = segmentTravelMinutes world step.Segment |> Math.Ceiling |> int
                                let delay = intersectionDelayMinutes world mode step.FromNode previousSegment step.Segment
                                let fromEndpoint = nodeEndpoint world step.FromNode |> Option.get
                                let toEndpoint = nodeEndpoint world step.ToNode |> Option.get

                                leg mode fromEndpoint toEndpoint step.LengthMeters (segmentMinutes + delay) (Some step.Segment.Id) step.Segment.LaneIds (Some step.FromNode) (Some step.ToNode) segmentMinutes delay)

                        let egressLegs =
                            [ if destinationAccess.AccessMeters > 0.0 then
                                  leg mode destinationNodeEndpoint destinationEndpoint destinationAccess.AccessMeters egressMinutes None [] (Some destinationAccess.Node) None egressMinutes 0 ]

                        let legs = accessLegs @ roadLegs @ egressLegs
                        let routeGeometry = routeGeometry legs
                        let segments = path.Steps |> List.map _.Segment
                        let segmentMinutes =
                            roadLegs
                            |> List.map _.SegmentTravelMinutes
                        let intersectionDelays =
                            roadLegs
                            |> List.map _.IntersectionDelayMinutes

                        Some
                            { TotalMinutes = float (legs |> List.sumBy _.ExpectedMinutes)
                              TotalDistanceMeters = routeGeometry.DistanceMeters
                              AccessMeters = originAccess.AccessMeters + destinationAccess.AccessMeters
                              Origin = originEndpoint
                              Destination = destinationEndpoint
                              Legs = legs
                              Geometry = routeGeometry
                              Segments = segments
                              NodePath = path.NodePath
                              SegmentTravelMinutes = segmentMinutes
                              IntersectionDelayMinutes = intersectionDelays }
                    | _ -> None)
            | _ -> None
        | _ -> None

    let route world mode origin destination =
        match mode with
        | Walk -> RouteFailed PedestrianNetworkUnavailable
        | Bike -> RouteFailed BikeNetworkUnavailable
        | PrivateCar
        | TaxiOrRideshare
        | Bus
        | SchoolBus
        | EmergencyVehicle
        | ServiceVehicle
        | DeliveryVehicle
        | FreightTruck ->
            match roadRoute world mode origin destination with
            | Some route -> RouteSucceeded route
            | None ->
                match MapGraph.resolveRoadAccess world.Map origin, MapGraph.resolveRoadAccess world.Map destination with
                | None, _ -> RouteFailed(RoadAccessUnavailable origin)
                | _, None -> RouteFailed(RoadAccessUnavailable destination)
                | Some originAccess, Some destinationAccess -> RouteFailed(RoadPathUnavailable(originAccess.Node, destinationAccess.Node))
        | mode -> RouteFailed(UnsupportedRouteMode mode)
