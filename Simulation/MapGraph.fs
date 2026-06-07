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
        $"road node {shortId}"

    let private segmentLength (cityMap: CityMap) (segment: RoadSegment) =
        if segment.LengthMeters > 0.0 then
            segment.LengthMeters
        else
            match Map.tryFind segment.From cityMap.RoadNodes, Map.tryFind segment.To cityMap.RoadNodes with
            | Some a, Some b -> distanceMeters cityMap a.Position b.Position
            | _ -> Double.PositiveInfinity

    let private nearestRoadNode (cityMap: CityMap) position maxDistanceMeters =
        cityMap.RoadNodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, node) -> nodeId, distanceMeters cityMap position node.Position)
        |> Seq.filter (fun (_, meters) -> meters <= maxDistanceMeters)
        |> Seq.sortBy snd
        |> Seq.tryHead

    let private resolveAccess (cityMap: CityMap) (place: Place) =
        match place.RoadAccess with
        | DirectRoadAccess nodeId ->
            cityMap.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map (fun node -> nodeId, distanceMeters cityMap place.Position node.Position)
        | NearestRoadAccess maxDistanceMeters ->
            nearestRoadNode cityMap place.Position maxDistanceMeters
        | NoRoadAccess -> None

    let private adjacency (cityMap: CityMap) =
        let addEdge fromNode edge map =
            let existing = Map.tryFind fromNode map |> Option.defaultValue []
            Map.add fromNode (edge :: existing) map

        ((Map.empty, cityMap.RoadSegments) ||> List.fold (fun map segment ->
            let length = segmentLength cityMap segment
            let minutes = minutesAtSpeed segment.SpeedKph length
            let edge = (segment.To, segment, length, minutes)
            let map = addEdge segment.From edge map

            if segment.IsTwoWay then
                let reverseEdge = (segment.From, segment, length, minutes)
                addEdge segment.To reverseEdge map
            else
                map))

    let private shortestRoadPath (cityMap: CityMap) originNode destinationNode =
        let graph = adjacency cityMap
        let nodes = cityMap.RoadNodes |> Map.toSeq |> Seq.map fst |> Set.ofSeq

        let rec loop unvisited distances previous =
            let current =
                unvisited
                |> Seq.choose (fun node ->
                    distances
                    |> Map.tryFind node
                    |> Option.map (fun distance -> node, distance))
                |> Seq.sortBy snd
                |> Seq.tryHead

            match current with
            | None -> None
            | Some (node, distance) when node = destinationNode ->
                Some (distances, previous)
            | Some (node, distance) when Double.IsPositiveInfinity distance ->
                None
            | Some (node, distance) ->
                let unvisited = Set.remove node unvisited
                let edges = Map.tryFind node graph |> Option.defaultValue []

                let distances, previous =
                    ((distances, previous), edges)
                    ||> List.fold (fun (distances, previous) (neighbor, segment, length, minutes) ->
                        if not (Set.contains neighbor unvisited) then
                            distances, previous
                        else
                            let candidate = distance + minutes
                            let known = Map.tryFind neighbor distances |> Option.defaultValue Double.PositiveInfinity

                            if candidate < known then
                                Map.add neighbor candidate distances, Map.add neighbor (node, segment, length, minutes) previous
                            else
                                distances, previous)

                loop unvisited distances previous

        let distances = Map.add originNode 0.0 Map.empty

        match loop nodes distances Map.empty with
        | None -> None
        | Some (distances, previous) ->
            let rec rebuild node steps =
                if node = originNode then
                    Some steps
                else
                    match Map.tryFind node previous with
                    | Some (prior, segment, length, minutes) ->
                        rebuild prior ((prior, node, segment, length, minutes) :: steps)
                    | None -> None

            rebuild destinationNode []
            |> Option.map (fun steps ->
                let total = Map.find destinationNode distances
                total, steps)

    let private directRoute (cityMap: CityMap) (origin: Place) (destination: Place) =
        let meters = distanceMeters cityMap origin.Position destination.Position
        let minutes = minutesAtSpeed localPlaneSpeedKph meters

        { Origin = origin.Id
          Destination = destination.Id
          Legs =
            [ { Mode = LocalPlane
                FromName = origin.Name
                ToName = destination.Name
                Meters = meters
                Minutes = minutes } ]
          TotalMeters = meters
          TotalMinutes = minutes
          UsesRoadNetwork = false }

    let findRoute (cityMap: CityMap) originId destinationId =
        match Map.tryFind originId cityMap.Places, Map.tryFind destinationId cityMap.Places with
        | Some origin, Some destination ->
            match resolveAccess cityMap origin, resolveAccess cityMap destination with
            | Some (originNode, originAccessMeters), Some (destinationNode, destinationAccessMeters) ->
                match shortestRoadPath cityMap originNode destinationNode with
                | Some (_, roadSteps) ->
                    let accessLegs =
                        [ if originAccessMeters > 0.0 then
                              { Mode = LocalPlane
                                FromName = origin.Name
                                ToName = roadNodeName originNode
                                Meters = originAccessMeters
                                Minutes = minutesAtSpeed localPlaneSpeedKph originAccessMeters } ]

                    let roadLegs =
                        roadSteps
                        |> List.map (fun (fromNode, toNode, segment, length, minutes) ->
                            { Mode = Road
                              FromName = roadNodeName fromNode
                              ToName = roadNodeName toNode
                              Meters = length
                              Minutes = minutes })

                    let egressLegs =
                        [ if destinationAccessMeters > 0.0 then
                              { Mode = LocalPlane
                                FromName = roadNodeName destinationNode
                                ToName = destination.Name
                                Meters = destinationAccessMeters
                                Minutes = minutesAtSpeed localPlaneSpeedKph destinationAccessMeters } ]

                    let legs = accessLegs @ roadLegs @ egressLegs
                    let totalMeters = legs |> List.sumBy _.Meters
                    let totalMinutes = legs |> List.sumBy _.Minutes

                    { Origin = origin.Id
                      Destination = destination.Id
                      Legs = legs
                      TotalMeters = totalMeters
                      TotalMinutes = totalMinutes
                      UsesRoadNetwork = true }
                | None -> directRoute cityMap origin destination
            | _ -> directRoute cityMap origin destination
            |> Some
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
