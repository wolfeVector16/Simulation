namespace Simulation

open System
open Simulation.Domain

module MapGraph =
    type TravelMode =
        | LocalPlane
        | Road

    type RouteLeg =
        { Mode: TravelMode
          FromName: string
          ToName: string
          Meters: float
          Minutes: float }

    type Route =
        { Origin: PlaceId
          Destination: PlaceId
          Legs: RouteLeg list
          TotalMeters: float
          TotalMinutes: float
          UsesRoadNetwork: bool }

    type RoadAccess =
        { Node: RoadNodeId
          AccessMeters: float }

    type RoadEdge =
        { FromNode: RoadNodeId
          ToNode: RoadNodeId
          Segment: RoadSegment
          LengthMeters: float }

    type RoadPathStep =
        { FromNode: RoadNodeId
          ToNode: RoadNodeId
          Segment: RoadSegment
          LengthMeters: float
          Cost: float }

    type RoadPath =
        { TotalCost: float
          Steps: RoadPathStep list
          NodePath: RoadNodeId list }

    let private localPlaneSpeedKph = 5.0

    let private distanceUnits a b =
        let dx = a.X - b.X
        let dy = a.Y - b.Y
        sqrt (dx * dx + dy * dy)

    let distanceMeters (cityMap: CityMap) (a: Coordinates) (b: Coordinates) =
        distanceUnits a b * cityMap.MetersPerMapUnit

    let private minutesAtSpeed speedKph meters =
        if speedKph <= 0.0 then
            Double.PositiveInfinity
        else
            (meters / 1000.0) / speedKph * 60.0

    let private placeName (cityMap: CityMap) placeId =
        cityMap.Places
        |> Map.tryFind placeId
        |> Option.map _.Name
        |> Option.defaultValue "Unknown place"

    let private roadNodeName (RoadNodeId id) =
        let shortId = id.ToString("N")[0..7]
        $"road node %s{shortId}"

    let segmentLength (cityMap: CityMap) (segment: RoadSegment) =
        if segment.LengthMeters > 0.0 then
            segment.LengthMeters
        else
            match Map.tryFind segment.From cityMap.RoadNodes, Map.tryFind segment.To cityMap.RoadNodes with
            | Some a, Some b -> distanceMeters cityMap a.Position b.Position
            | _ -> Double.PositiveInfinity

    let nearestRoadNode (cityMap: CityMap) position maxDistanceMeters =
        cityMap.RoadNodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, node) -> nodeId, distanceMeters cityMap position node.Position)
        |> Seq.filter (fun (_, meters) -> meters <= maxDistanceMeters)
        |> Seq.sortBy snd
        |> Seq.tryHead

    let resolveRoadAccessForPlace (cityMap: CityMap) (place: Place) =
        match place.RoadAccess with
        | DirectRoadAccess nodeId ->
            cityMap.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map (fun node -> { Node = nodeId; AccessMeters = distanceMeters cityMap place.Position node.Position })
        | NearestRoadAccess maxDistanceMeters ->
            nearestRoadNode cityMap place.Position maxDistanceMeters
            |> Option.map (fun (nodeId, meters) -> { Node = nodeId; AccessMeters = meters })
        | NoRoadAccess -> None

    let resolveRoadAccess (cityMap: CityMap) placeId =
        cityMap.Places
        |> Map.tryFind placeId
        |> Option.bind (fun place -> resolveRoadAccessForPlace cityMap place)

    let roadAdjacency (cityMap: CityMap) =
        let addEdge fromNode edge map =
            let existing = Map.tryFind fromNode map |> Option.defaultValue []
            Map.add fromNode (edge :: existing) map

        ((Map.empty, cityMap.RoadSegments) ||> List.fold (fun map segment ->
            let length = segmentLength cityMap segment
            let edge = { FromNode = segment.From; ToNode = segment.To; Segment = segment; LengthMeters = length }
            let map = addEdge segment.From edge map

            if segment.IsTwoWay then
                addEdge segment.To { edge with FromNode = segment.To; ToNode = segment.From } map
            else
                map))

    let nodePathFromSteps steps =
        match steps with
        | [] -> []
        | first :: _ -> first.FromNode :: (steps |> List.map _.ToNode)

    let routePolyline (cityMap: CityMap) nodePath =
        nodePath
        |> List.choose (fun nodeId ->
            cityMap.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map _.Position)

    let shortestRoadPathWithCost (cityMap: CityMap) originNode destinationNode edgeCost =
        let graph = roadAdjacency cityMap
        let segmentById = cityMap.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList
        let startState = originNode, None
        let allStates =
            seq {
                yield startState

                for nodeId in cityMap.RoadNodes |> Map.toSeq |> Seq.map fst do
                    yield nodeId, None

                    for segment in cityMap.RoadSegments do
                        if segment.From = nodeId || segment.To = nodeId then
                            yield nodeId, Some segment.Id
            }
            |> Set.ofSeq

        let rec loop unvisited distances previous =
            let current =
                unvisited
                |> Seq.choose (fun state -> distances |> Map.tryFind state |> Option.map (fun distance -> state, distance))
                |> Seq.sortBy snd
                |> Seq.tryHead

            match current with
            | None -> None
            | Some ((node, _), _) when node = destinationNode ->
                Some (distances, previous)
            | Some (_, distance) when Double.IsPositiveInfinity distance ->
                None
            | Some ((node, previousSegmentId), distance) ->
                let state = node, previousSegmentId
                let unvisited = Set.remove state unvisited
                let previousSegment = previousSegmentId |> Option.bind (fun segmentId -> Map.tryFind segmentId segmentById)
                let edges = Map.tryFind node graph |> Option.defaultValue []

                let distances, previous =
                    ((distances, previous), edges |> List.sortBy (fun edge -> edge.ToNode, edge.Segment.Id))
                    ||> List.fold (fun (distances, previous) edge ->
                        let nextState = edge.ToNode, Some edge.Segment.Id

                        if not (Set.contains nextState unvisited) then
                            distances, previous
                        else
                            let cost = edgeCost node previousSegment edge
                            let candidate = distance + cost
                            let known = Map.tryFind nextState distances |> Option.defaultValue Double.PositiveInfinity

                            if candidate < known then
                                Map.add nextState candidate distances, Map.add nextState (state, edge, cost) previous
                            else
                                distances, previous)

                loop unvisited distances previous

        let distances = Map.add startState 0.0 Map.empty

        match loop allStates distances Map.empty with
        | None -> None
        | Some (distances, previous) ->
            let destinationState =
                distances
                |> Map.toSeq
                |> Seq.filter (fun ((node, _), _) -> node = destinationNode)
                |> Seq.sortBy snd
                |> Seq.tryHead
                |> Option.map fst

            let rec rebuild state steps =
                if state = startState then
                    Some steps
                else
                    match Map.tryFind state previous with
                    | Some (priorState, edge, cost) ->
                        let step =
                            { FromNode = edge.FromNode
                              ToNode = edge.ToNode
                              Segment = edge.Segment
                              LengthMeters = edge.LengthMeters
                              Cost = cost }

                        rebuild priorState (step :: steps)
                    | None -> None

            destinationState
            |> Option.bind (fun destinationState ->
                rebuild destinationState []
                |> Option.map (fun steps ->
                    { TotalCost = Map.find destinationState distances
                      Steps = steps
                      NodePath = nodePathFromSteps steps }))

    let shortestRoadPath cityMap originNode destinationNode =
        shortestRoadPathWithCost cityMap originNode destinationNode (fun _ _ edge ->
            minutesAtSpeed edge.Segment.SpeedKph edge.LengthMeters)

    let findRoute (cityMap: CityMap) originId destinationId =
        match Map.tryFind originId cityMap.Places, Map.tryFind destinationId cityMap.Places with
        | Some origin, Some destination ->
            match resolveRoadAccessForPlace cityMap origin, resolveRoadAccessForPlace cityMap destination with
            | Some originAccess, Some destinationAccess ->
                match shortestRoadPath cityMap originAccess.Node destinationAccess.Node with
                | Some roadPath ->
                    let accessLegs =
                        [ if originAccess.AccessMeters > 0.0 then
                              { Mode = LocalPlane
                                FromName = origin.Name
                                ToName = roadNodeName originAccess.Node
                                Meters = originAccess.AccessMeters
                                Minutes = minutesAtSpeed localPlaneSpeedKph originAccess.AccessMeters } ]

                    let roadLegs =
                        roadPath.Steps
                        |> List.map (fun step ->
                            { Mode = Road
                              FromName = roadNodeName step.FromNode
                              ToName = roadNodeName step.ToNode
                              Meters = step.LengthMeters
                              Minutes = step.Cost })

                    let egressLegs =
                        [ if destinationAccess.AccessMeters > 0.0 then
                              { Mode = LocalPlane
                                FromName = roadNodeName destinationAccess.Node
                                ToName = destination.Name
                                Meters = destinationAccess.AccessMeters
                                Minutes = minutesAtSpeed localPlaneSpeedKph destinationAccess.AccessMeters } ]

                    let legs = accessLegs @ roadLegs @ egressLegs
                    let totalMeters = legs |> List.sumBy _.Meters
                    let totalMinutes = legs |> List.sumBy _.Minutes

                    { Origin = origin.Id
                      Destination = destination.Id
                      Legs = legs
                      TotalMeters = totalMeters
                      TotalMinutes = totalMinutes
                      UsesRoadNetwork = true }
                    |> Some
                | None -> None
            | _ -> None
        | _ -> None

    let routeMinutes (cityMap: CityMap) originId destinationId =
        findRoute cityMap originId destinationId
        |> Option.map (fun route -> max 1 (int (Math.Ceiling route.TotalMinutes)))

    let describeRoute (cityMap: CityMap) route =
        let mode =
            if route.UsesRoadNetwork then
                "road graph"
            else
                "local plane"

        sprintf
            "%s -> %s via %s: %.1f km, %.0f min"
            (placeName cityMap route.Origin)
            (placeName cityMap route.Destination)
            mode
            (route.TotalMeters / 1000.0)
            (Math.Ceiling route.TotalMinutes)
