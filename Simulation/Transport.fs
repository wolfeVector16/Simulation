namespace Simulation

open System
open System.Security.Cryptography
open System.Text
open Simulation.Domain
open Simulation.Measures

module Transport =
    let private stableGuid parts =
        let text = String.concat "|" parts
        let bytes = Encoding.UTF8.GetBytes text
        let hash = SHA256.HashData bytes
        let guidBytes = Array.zeroCreate<byte> 16
        Array.Copy(hash, guidBytes, 16)
        Guid(guidBytes)

    let private tripId seed tick label key =
        TransportTripId(stableGuid [ string seed; string tick; label; key ])

    let private routeId seed tick label key =
        TransportRouteId(stableGuid [ "route"; string seed; string tick; label; key ])

    let private vehicleId seed tick label key =
        VehicleId(stableGuid [ "vehicle"; string seed; string tick; label; key ])

    let private clampInt low high value =
        value |> max low |> min high

    let private minutesAtSpeed speedKph meters =
        if speedKph <= 0.0 then
            Int32.MaxValue
        else
            max 1 (int (Math.Ceiling((meters / 1000.0) / speedKph * 60.0)))

    let private placeName world placeId =
        world.Map.Places
        |> Map.tryFind placeId
        |> Option.map _.Name
        |> Option.defaultValue "unknown"

    let private locationPlace =
        function
        | PlaceRef placeId -> Some placeId
        | _ -> None

    let private distanceMeters world origin destination =
        match Map.tryFind origin world.Map.Places, Map.tryFind destination world.Map.Places with
        | Some a, Some b -> MapGraph.distanceMeters world.Map a.Position b.Position
        | _ -> Double.PositiveInfinity

    let private nearestRoadNode world position maxDistanceMeters =
        world.Map.RoadNodes
        |> Map.toSeq
        |> Seq.map (fun (nodeId, node) -> nodeId, MapGraph.distanceMeters world.Map position node.Position)
        |> Seq.filter (fun (_, meters) -> meters <= maxDistanceMeters)
        |> Seq.sortBy snd
        |> Seq.tryHead

    let private resolveRoadAccess world placeId =
        match Map.tryFind placeId world.Map.Places with
        | None -> None
        | Some place ->
            match place.RoadAccess with
            | DirectRoadAccess nodeId -> Some(nodeId, 0.0)
            | NearestRoadAccess maxMeters -> nearestRoadNode world place.Position maxMeters
            | NoRoadAccess -> None

    let private segmentLength world (segment: RoadSegment) =
        if segment.LengthMeters > 0.0 then
            segment.LengthMeters
        else
            match Map.tryFind segment.From world.Map.RoadNodes, Map.tryFind segment.To world.Map.RoadNodes with
            | Some a, Some b -> MapGraph.distanceMeters world.Map a.Position b.Position
            | _ -> Double.PositiveInfinity

    let private congestionFor world (segment: RoadSegment) =
        world.Transport.SegmentCongestion
        |> Map.tryFind segment.Id
        |> Option.defaultValue 0.0

    type private DirectedRoadEdge =
        { FromNode: RoadNodeId
          ToNode: RoadNodeId
          Segment: RoadSegment
          LengthMeters: float
          SegmentMinutes: float }

    type private RoadStep =
        { FromNode: RoadNodeId
          ToNode: RoadNodeId
          Segment: RoadSegment
          LengthMeters: float
          SegmentMinutes: int
          IntersectionDelayMinutes: int
          Movement: IntersectionMovement }

    let private segmentEffectiveSpeedKph world (segment: RoadSegment) =
        let congestion = congestionFor world segment
        segment.SpeedKph * (1.0 - min 0.75 (congestion * 0.55)) * segment.SurfaceCondition * (1.0 - segment.WeatherImpact)

    let private segmentTravelMinutes world (segment: RoadSegment) =
        let length = segmentLength world segment
        let incidentPenalty =
            if segment.CurrentIncidents.IsEmpty && not segment.UnderConstruction then 1.0
            else 1.0 + max 0.25 (float segment.CurrentIncidents.Count * 0.35)

        minutesAtSpeed (segmentEffectiveSpeedKph world segment) length |> float |> (*) incidentPenalty

    let private roadAdjacency world =
        let addEdge fromNode edge map =
            let existing = Map.tryFind fromNode map |> Option.defaultValue []
            Map.add fromNode (edge :: existing) map

        ((Map.empty, world.Map.RoadSegments) ||> List.fold (fun map segment ->
            let length = segmentLength world segment
            let minutes = segmentTravelMinutes world segment
            let edge = { FromNode = segment.From; ToNode = segment.To; Segment = segment; LengthMeters = length; SegmentMinutes = minutes }
            let map = addEdge segment.From edge map

            if segment.IsTwoWay then
                addEdge segment.To { edge with FromNode = segment.To; ToNode = segment.From } map
            else
                map))

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

    let private roadRoute world mode origin destination =
        match resolveRoadAccess world origin, resolveRoadAccess world destination with
        | Some (originNode, originAccess), Some (destinationNode, destinationAccess) ->
            let graph = roadAdjacency world
            let segmentById = world.Map.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList
            let startState = originNode, None
            let allStates =
                seq {
                    yield startState

                    for nodeId in world.Map.RoadNodes |> Map.toSeq |> Seq.map fst do
                        yield nodeId, None

                        for segment in world.Map.RoadSegments do
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
                | Some ((node, _), _) when node = destinationNode -> Some(distances, previous)
                | Some (_, distance) when Double.IsPositiveInfinity distance -> None
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
                                let delay = intersectionDelayMinutes world mode node previousSegment edge.Segment
                                let candidate = distance + edge.SegmentMinutes + float delay
                                let known = Map.tryFind nextState distances |> Option.defaultValue Double.PositiveInfinity

                                if candidate < known then
                                    Map.add nextState candidate distances,
                                    Map.add nextState (state, edge, delay, classifyIntersectionMovement world node previousSegment edge.Segment) previous
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
                        | Some (priorState, edge, delay, movement) ->
                            let step =
                                { FromNode = edge.FromNode
                                  ToNode = edge.ToNode
                                  Segment = edge.Segment
                                  LengthMeters = edge.LengthMeters
                                  SegmentMinutes = int (Math.Ceiling edge.SegmentMinutes)
                                  IntersectionDelayMinutes = delay
                                  Movement = movement }

                            rebuild priorState (step :: steps)
                        | None -> None

                destinationState
                |> Option.bind (fun destinationState ->
                    rebuild destinationState []
                    |> Option.map (fun steps ->
                        let totalRoadMinutes = Map.find destinationState distances
                        let accessMinutes = float (minutesAtSpeed 5.0 (originAccess + destinationAccess))
                        let segments = steps |> List.map _.Segment
                        let segmentMinutes = steps |> List.map _.SegmentMinutes
                        let intersectionDelays = steps |> List.map _.IntersectionDelayMinutes
                        let nodePath =
                            match steps with
                            | [] -> []
                            | first :: _ -> first.FromNode :: (steps |> List.map _.ToNode)

                        totalRoadMinutes + accessMinutes, originAccess + destinationAccess, segments, nodePath, segmentMinutes, intersectionDelays))
        | _ -> None

    let private firstParkingNear world destination =
        world.Transport.ParkingZones
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun zone -> zone.NearPlace = Some destination)
        |> Seq.sortBy (fun zone -> zone.PricePerHour, zone.AverageSearchMinutes)
        |> Seq.tryHead

    let private transitRouteServing world origin destination =
        let stopPlaces =
            world.Transport.TransitStops
            |> Map.toSeq
            |> Seq.choose (fun (stopId, stop) -> stop.Place |> Option.map (fun placeId -> placeId, stopId))
            |> Map.ofSeq

        match Map.tryFind origin stopPlaces, Map.tryFind destination stopPlaces with
        | Some originStop, Some destinationStop ->
            world.Transport.TransitRoutes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.tryFind (fun (route: TransitRoute) ->
                let stops = route.Stops
                match List.tryFindIndex ((=) originStop) stops, List.tryFindIndex ((=) destinationStop) stops with
                | Some fromIndex, Some toIndex -> fromIndex < toIndex || toIndex < fromIndex
                | _ -> false)
        | _ -> None

    let private neighborhoodForHousehold world householdId =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.tryFind (fun (_, unit) -> Set.contains householdId unit.Occupants)
        |> Option.map (fun (_, unit) -> unit.Neighborhood)

    let private accessForHousehold world household =
        neighborhoodForHousehold world household.Id
        |> Option.bind (fun neighborhoodId -> Map.tryFind neighborhoodId world.Transport.AccessByNeighborhood)

    let private availableModes world (sim: Sim) (household: Household) origin destination =
        let directMeters = distanceMeters world origin destination
        let access = accessForHousehold world household

        let walkLimit = if sim.LifeStage = Child then 1800.0 else 3200.0
        let bikeSafety = access |> Option.map _.BikeSafety |> Option.defaultValue 0.35

        [ if directMeters <= walkLimit then Walk
          if directMeters <= 9000.0 && bikeSafety > 0.45 && sim.LifeStage <> Child then Bike
          if sim.LifeStage <> Child && (household.TransportationAccess >= 0.55 || household.Assets > 5000m) then PrivateCar
          if transitRouteServing world origin destination |> Option.isSome then Bus ]
        |> Set.ofList

    let private driverProfile (sim: Sim) =
        { Aggressiveness = clamp01 (0.25 + (1.0 - sim.Personality.Agreeableness) * 0.40 + sim.Personality.Ambition * 0.15)
          Patience = clamp01 (0.30 + sim.Personality.Agreeableness * 0.35 + sim.Personality.RoutinePreference * 0.20)
          Familiarity = clamp01 (0.45 + sim.Personality.RoutinePreference * 0.35)
          RiskTolerance = clamp01 (0.25 + sim.Personality.Openness * 0.25 + (1.0 - sim.Personality.Neuroticism) * 0.20)
          LawCompliance = clamp01 (0.45 + sim.Personality.Conscientiousness * 0.45)
          StressLevel = clamp01 (1.0 - sim.Happiness)
          Urgency = clamp01 (0.25 + sim.Personality.Ambition * 0.55)
          HighwayAversion = clamp01 (0.15 + sim.Personality.Neuroticism * 0.40)
          TollAversion = clamp01 (0.20 + sim.Personality.Frugality * 0.60)
          RerouteTendency = clamp01 (0.25 + sim.Personality.Openness * 0.35)
          WalkingToleranceMeters = 900.0 + sim.Personality.Openness * 1800.0
          TransitTolerance = clamp01 (0.35 + sim.Personality.Frugality * 0.35 + sim.Personality.RoutinePreference * 0.15) }

    let private tripPurposeFromSimple purpose =
        match purpose with
        | ToWork
        | ToHome -> WorkTrip
        | ToSchool
        | FromSchool -> SchoolTrip
        | ToDaycare
        | FromDaycare -> SchoolPickupDropoffTrip
        | ToShopping _ -> ShoppingTrip
        | ToErrand -> InstitutionalAppointmentTrip
        | ToLeisure -> RecreationTrip

    let private deadlineForSimple (sim: Sim) purpose =
        match purpose, sim.Job, sim.School with
        | ToWork, Some job, _ -> Some job.StartMinute
        | ToSchool, _, Some school -> Some school.StartMinute
        | ToDaycare, _, Some school -> Some school.StartMinute
        | _ -> None

    let private privateCarRoute world seed tick tripKey origin destination deadline (sim: Sim) _household =
        roadRoute world PrivateCar origin destination
        |> Option.map (fun (roadMinutes, accessMeters, segments, nodePath, segmentMinutes, intersectionDelays) ->
            let parking = firstParkingNear world destination
            let parkingPressure =
                parking
                |> Option.map (fun zone -> float zone.Occupied / max 1.0 (float zone.Capacity))
                |> Option.defaultValue 0.85

            let parkingMinutes = parking |> Option.map _.AverageSearchMinutes |> Option.defaultValue 12
            let parkingCost = parking |> Option.map (fun zone -> zone.PricePerHour) |> Option.defaultValue 4m
            let congestion = segments |> List.map (congestionFor world) |> List.append [ 0.0 ] |> List.average
            let expected = int (Math.Ceiling roadMinutes) + parkingMinutes
            let lateRisk =
                deadline
                |> Option.map (fun deadline -> normalizeMinute (world.MinuteOfDay + expected) - deadline)
                |> Option.defaultValue 0

            let reasons =
                [ if sim.Dependents.Length > 0 then NeedsChildPickup
                  if lateRisk > -10 then DeadlinePressure
                  if transitRouteServing world origin destination |> Option.exists (fun route -> route.Reliability < 0.70) then TransitUnreliable
                  if parkingPressure > 0.82 then ParkingUnavailable
                  if parkingCost > 6m then ParkingTooExpensive
                  if congestion > 0.55 then HeavyCongestion ]

            let route =
                { Id = routeId seed tick "private-car" tripKey
                  Mode = PrivateCar
                  SegmentIds = segments |> List.map _.Id
                  LaneIds = segments |> List.collect _.LaneIds
                  NodePath = nodePath
                  SegmentTravelMinutes = segmentMinutes
                  IntersectionDelayMinutes = intersectionDelays
                  TransitRouteId = None
                  ExpectedMinutes = expected
                  Reliability = clamp01 (0.90 - congestion * 0.35 - parkingPressure * 0.18)
                  MoneyCost = parkingCost + decimal (roadMinutes * 0.11)
                  WalkMeters = accessMeters + (parking |> Option.map _.WalkingDistanceMeters |> Option.defaultValue 300.0)
                  Safety = clamp01 (0.80 - congestion * 0.15)
                  Stress = clamp01 (congestion * 0.45 + parkingPressure * 0.35 + sim.Personality.Neuroticism * 0.20)
                  RequiresParking = true
                  TransferCount = 0 }

            route, roadMinutes + float parkingMinutes, reasons)

    let private busRoute world seed tick tripKey origin destination deadline (sim: Sim) =
        transitRouteServing world origin destination
        |> Option.bind (fun transit ->
            roadRoute world Bus origin destination
            |> Option.map (fun (roadMinutes, accessMeters, segments, nodePath, segmentMinutes, intersectionDelays) ->
                let trafficPenalty = if transit.DedicatedRightOfWay then 1.0 else 1.0 + (segments |> List.map (congestionFor world) |> List.append [ 0.0 ] |> List.average) * 0.45
                let wait = max 1 (transit.HeadwayMinutes / 2)
                let dwell = max 2 (transit.Stops.Length * 1)
                let expected = int (Math.Ceiling(roadMinutes * trafficPenalty)) + wait + dwell
                let arrival = normalizeMinute (world.MinuteOfDay + expected)
                let lateRisk = deadline |> Option.map (fun d -> arrival - d) |> Option.defaultValue 0

                let reasons =
                    [ if transit.Reliability < 0.70 then TransitUnreliable
                      if world.MinuteOfDay < transit.ServiceStartMinute || world.MinuteOfDay > transit.ServiceEndMinute then TransitUnavailable
                      if lateRisk > -5 then DeadlinePressure
                      if transit.Crowding > 0.80 then MissedConnectionRisk ]

                let route =
                    { Id = routeId seed tick "bus" tripKey
                      Mode = Bus
                      SegmentIds = segments |> List.map _.Id
                      LaneIds = segments |> List.collect _.LaneIds
                      NodePath = nodePath
                      SegmentTravelMinutes = segmentMinutes
                      IntersectionDelayMinutes = intersectionDelays
                      TransitRouteId = Some transit.Id
                      ExpectedMinutes = expected
                      Reliability = clamp01 (transit.Reliability - transit.Crowding * 0.15)
                      MoneyCost = transit.Fare
                      WalkMeters = accessMeters + 350.0
                      Safety = clamp01 (0.72 + transit.Reliability * 0.15)
                      Stress = clamp01 ((1.0 - transit.Reliability) * 0.45 + transit.Crowding * 0.35 + sim.Personality.Neuroticism * 0.20)
                      RequiresParking = false
                      TransferCount = 0 }

                route, float expected, reasons))

    let private simpleModeRoute world seed tick tripKey mode origin destination (sim: Sim) =
        let meters = distanceMeters world origin destination

        if Double.IsInfinity meters then
            None
        else
            let speed, baseSafety, extraReasons =
                match mode with
                | Walk ->
                    let driver = driverProfile sim
                    4.8, 0.62, [ if meters > driver.WalkingToleranceMeters then MobilityLimitation ]
                | Bike -> 15.0, 0.55, [ FamiliarRoute ]
                | _ -> 1.0, 0.0, []

            let expected = minutesAtSpeed speed meters
            let route =
                { Id = routeId seed tick (sprintf "%A" mode) tripKey
                  Mode = mode
                  SegmentIds = []
                  LaneIds = []
                  NodePath = []
                  SegmentTravelMinutes = []
                  IntersectionDelayMinutes = []
                  TransitRouteId = None
                  ExpectedMinutes = expected
                  Reliability = 0.78
                  MoneyCost = 0m
                  WalkMeters = if mode = Walk then meters else 120.0
                  Safety = baseSafety
                  Stress = clamp01 ((1.0 - baseSafety) * 0.45 + sim.Personality.Neuroticism * 0.25)
                  RequiresParking = false
                  TransferCount = 0 }

            Some(route, float expected, extraReasons)

    let private chooseModeAndRoute world seed tick tripKey (sim: Sim) household origin destination _ deadline available =
        let candidates =
            available
            |> Seq.sort
            |> Seq.truncate world.Performance.MaxRouteAlternativesPerTrip
            |> Seq.choose (fun mode ->
                match mode with
                | PrivateCar -> privateCarRoute world seed tick tripKey origin destination deadline sim household
                | Bus -> busRoute world seed tick tripKey origin destination deadline sim
                | Walk
                | Bike -> simpleModeRoute world seed tick tripKey mode origin destination sim
                | _ -> None
                |> Option.map (fun (route, minutes, reasons) ->
                    let deadlinePenalty =
                        deadline
                        |> Option.map (fun d -> max 0 (normalizeMinute (world.MinuteOfDay + route.ExpectedMinutes) - d) |> float)
                        |> Option.defaultValue 0.0

                    let generalizedCost =
                        minutes
                        + (float route.MoneyCost * 0.35)
                        + ((1.0 - route.Reliability) * 28.0)
                        + (route.Stress * 18.0)
                        + (deadlinePenalty * 2.0)

                    mode, route, reasons, generalizedCost))
            |> Seq.sortBy (fun (_, route, _, cost) -> cost, route.ExpectedMinutes)
            |> Seq.toList

        candidates |> List.tryHead

    let private simTransportDemand world (simId, sim: Sim) =
        match sim.Location with
        | InTransit trip ->
            match Map.tryFind sim.Household world.Households with
            | None -> None
            | Some household ->
                let activeExists =
                    world.Transport.Trips
                    |> Map.toSeq
                    |> Seq.exists (fun (_, transportTrip) -> transportTrip.PersonId = Some simId && transportTrip.Status = InProgress)

                if activeExists then
                    None
                else
                    let origin = trip.Origin
                    let destination = trip.Destination
                    let available = availableModes world sim household origin destination

                    if available.IsEmpty then
                        None
                    else
                        let key =
                            match simId with
                            | SimId id -> id.ToString("N")

                        let purpose = tripPurposeFromSimple trip.Purpose
                        let deadline = deadlineForSimple sim trip.Purpose
                        let tripId = tripId world.Meta.Seed world.Meta.Tick "person-trip" key
                        let chosen = chooseModeAndRoute world world.Meta.Seed world.Meta.Tick key sim household origin destination purpose deadline available

                        chosen
                        |> Option.map (fun (mode, route, reasons, _) ->
                            let transportTrip =
                                { Id = tripId
                                  PersonId = Some simId
                                  HouseholdId = Some sim.Household
                                  Purpose = purpose
                                  Origin = PlaceRef origin
                                  Destination = PlaceRef destination
                                  DeadlineMinute = deadline
                                  AvailableModes = available
                                  ChosenMode = Some mode
                                  ModeChoiceReasons = reasons
                                  PlannedRoute = Some route
                                  CurrentRoute = Some route
                                  FallbackModes = available |> Set.remove mode |> Set.toList
                                  ToleranceForDelayMinutes = if purpose = WorkTrip || purpose = SchoolTrip then 5 else 20
                                  WillingnessToReroute = driverProfile sim |> _.RerouteTendency
                                  Stress = route.Stress
                                  Status = InProgress
                                  ChainIndex = 0
                                  ChainLength = if sim.Dependents.IsEmpty then 1 else 2 }

                            let driver = driverProfile sim
                            let firstSegmentId = route.SegmentIds |> List.tryHead
                            let firstLaneId =
                                firstSegmentId
                                |> Option.bind (fun segmentId ->
                                    world.Map.RoadSegments
                                    |> List.tryFind (fun segment -> segment.Id = segmentId)
                                    |> Option.bind (fun segment -> segment.LaneIds |> List.tryHead))

                            let currentSpeed =
                                firstSegmentId
                                |> Option.bind (fun segmentId -> world.Map.RoadSegments |> List.tryFind (fun segment -> segment.Id = segmentId))
                                |> Option.map (fun segment -> segmentEffectiveSpeedKph world segment)
                                |> Option.defaultValue 0.0

                            let vehicle =
                                { Id = vehicleId world.Meta.Seed world.Meta.Tick "person-vehicle" key
                                  Trip = tripId
                                  Mode = mode
                                  CurrentPosition =
                                    firstSegmentId
                                    |> Option.map (fun segmentId -> OnRoadSegment(segmentId, firstLaneId, 0.0))
                                    |> Option.defaultValue OffNetwork
                                  PreviousPosition = None
                                  CurrentSpeedKph = currentSpeed
                                  CurrentRouteIndex = firstSegmentId |> Option.map (fun _ -> 0)
                                  Status = if firstSegmentId.IsSome then VehicleMoving else VehicleCompleted
                                  CurrentLane = firstLaneId
                                  NextRequiredMovement = Some MoveRight
                                  DistanceToManeuverMeters = route.SegmentIds.Length |> float |> (*) 500.0
                                  Driver = driver
                                  MissedManeuvers = 0
                                  DelayMinutes = 0
                                  Occupants = Some 1 }

                            transportTrip, vehicle, route, reasons)
        | _ -> None

    let private completeArrivedTrips world =
        let inTransitPeople =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (fun (simId, sim) -> match sim.Location with InTransit _ -> Some simId | _ -> None)
            |> Set.ofSeq

        world.Transport.Trips
        |> Map.toSeq
        |> Seq.choose (fun (tripId, trip) ->
            match trip.Status, trip.PersonId with
            | InProgress, Some simId when not (Set.contains simId inTransitPeople) ->
                let delay =
                    match trip.DeadlineMinute, trip.CurrentRoute with
                    | Some deadline, Some _ -> max 0 (normalizeMinute (world.MinuteOfDay) - deadline)
                    | _ -> 0

                let events =
                    [ TripCompleted tripId
                      if delay > trip.ToleranceForDelayMinutes then
                          ArrivedLate(simId, trip.Purpose, delay) ]

                Some(tripId, { trip with Status = Completed }, events)
            | _ -> None)
        |> Seq.toList

    let private parkingEvents world (trip: TransportTrip) (route: TransportRoute) =
        if not route.RequiresParking then
            []
        else
            match locationPlace trip.Destination |> Option.bind (fun place -> firstParkingNear world place) with
            | None -> [ ParkingSearchStarted trip.Id; ParkingFailed trip.Id ]
            | Some zone ->
                let pressure = float zone.Occupied / max 1.0 (float zone.Capacity)
                if pressure > 0.94 then
                    [ ParkingSearchStarted trip.Id; ParkingFailed trip.Id ]
                else
                    [ ParkingSearchStarted trip.Id; ParkingFound(trip.Id, zone.Id) ]

    let private laneBehaviorEvents world (vehicle: VehicleState) (route: TransportRoute) =
        match vehicle.CurrentLane, route.LaneIds |> List.tryLast with
        | Some currentLane, Some targetLane when currentLane <> targetLane ->
            let congestion =
                route.SegmentIds
                |> List.map (fun segmentId -> world.Transport.SegmentCongestion |> Map.tryFind segmentId |> Option.defaultValue 0.0)
                |> List.append [ 0.0 ]
                |> List.average

            let mergePressure = congestion * (1.0 - vehicle.Driver.Patience) + vehicle.Driver.StressLevel * 0.25

            if mergePressure > 0.58 then
                [ LaneChangeFailed(vehicle.Id, currentLane, targetLane) ]
            else
                [ LaneChanged(vehicle.Id, currentLane, targetLane) ]
        | _ -> []

    let private routeSegmentLane world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.bind (fun segment -> segment.LaneIds |> List.tryHead)

    let private routeSegmentLength world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.map (fun segment -> segmentLength world segment)
        |> Option.defaultValue Double.PositiveInfinity

    let private routeSegmentSpeed world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.map (fun segment -> segmentEffectiveSpeedKph world segment)
        |> Option.defaultValue 0.0

    let private routeIntersectionAfterSegment route index =
        route.NodePath
        |> List.tryItem (index + 1)

    let private completeVehicle world (trip: TransportTrip) (route: TransportRoute) (vehicle: VehicleState) previousPosition =
        let destination = locationPlace trip.Destination

        let parking =
            if route.RequiresParking then
                destination |> Option.bind (fun place -> firstParkingNear world place) |> Option.map _.Id
            else
                None

        { vehicle with
            PreviousPosition = previousPosition
            CurrentPosition = if route.RequiresParking then ParkedAt(parking, destination) else CompletedTripPosition
            CurrentSpeedKph = 0.0
            CurrentRouteIndex = None
            Status = if route.RequiresParking then VehicleParked else VehicleCompleted
            CurrentLane = None
            DelayMinutes = 0 }

    let private enterRouteSegment world route index (vehicle: VehicleState) previousPosition =
        match route.SegmentIds |> List.tryItem index with
        | None ->
            { vehicle with
                PreviousPosition = previousPosition
                CurrentPosition = CompletedTripPosition
                CurrentSpeedKph = 0.0
                CurrentRouteIndex = None
                Status = VehicleCompleted
                CurrentLane = None
                DelayMinutes = 0 }
        | Some segmentId ->
            let lane = routeSegmentLane world segmentId

            { vehicle with
                PreviousPosition = previousPosition
                CurrentPosition = OnRoadSegment(segmentId, lane, 0.0)
                CurrentSpeedKph = routeSegmentSpeed world segmentId
                CurrentRouteIndex = Some index
                Status = VehicleMoving
                CurrentLane = lane
                DelayMinutes = 0 }

    let private advanceVehicle minutes world transport (vehicle: VehicleState) =
        match Map.tryFind vehicle.Trip transport.Trips, vehicle.CurrentRouteIndex with
        | Some trip, Some index ->
            match trip.CurrentRoute with
            | None -> vehicle
            | Some route ->
                match vehicle.Status, vehicle.CurrentPosition with
                | VehicleWaitingAtIntersection, WaitingAtIntersection _ ->
                    let remaining = max 0 (vehicle.DelayMinutes - minutes)

                    if remaining > 0 then
                        { vehicle with
                            PreviousPosition = Some vehicle.CurrentPosition
                            DelayMinutes = remaining
                            CurrentSpeedKph = 0.0 }
                    else
                        let nextIndex = index + 1

                        if nextIndex >= route.SegmentIds.Length then
                            completeVehicle world trip route vehicle (Some vehicle.CurrentPosition)
                        else
                            enterRouteSegment world route nextIndex vehicle (Some vehicle.CurrentPosition)
                | VehicleMoving, OnRoadSegment(segmentId, laneId, progress) ->
                    let speed = routeSegmentSpeed world segmentId
                    let length = routeSegmentLength world segmentId

                    if Double.IsInfinity length || length <= 0.0 || speed <= 0.0 then
                        { vehicle with
                            PreviousPosition = Some vehicle.CurrentPosition
                            CurrentSpeedKph = 0.0
                            Status = VehicleQueued }
                    else
                        let metersThisTick = speed * 1000.0 / 60.0 * float minutes
                        let nextProgress = progress + metersThisTick / length

                        if nextProgress < 1.0 then
                            { vehicle with
                                PreviousPosition = Some vehicle.CurrentPosition
                                CurrentPosition = OnRoadSegment(segmentId, laneId, clamp01 nextProgress)
                                CurrentSpeedKph = speed
                                Status = VehicleMoving }
                        else
                            let nextIndex = index + 1

                            if nextIndex >= route.SegmentIds.Length then
                                completeVehicle world trip route vehicle (Some vehicle.CurrentPosition)
                            else
                                let delay =
                                    route.IntersectionDelayMinutes
                                    |> List.tryItem nextIndex
                                    |> Option.defaultValue 0

                                if delay > 0 then
                                    let intersectionNode = routeIntersectionAfterSegment route index

                                    { vehicle with
                                        PreviousPosition = Some vehicle.CurrentPosition
                                        CurrentPosition =
                                            intersectionNode
                                            |> Option.map (fun nodeId -> WaitingAtIntersection(nodeId, laneId))
                                            |> Option.defaultValue OffNetwork
                                        CurrentSpeedKph = 0.0
                                        Status = VehicleWaitingAtIntersection
                                        DelayMinutes = delay }
                                else
                                    enterRouteSegment world route nextIndex vehicle (Some vehicle.CurrentPosition)
                | _ -> vehicle
        | _ -> vehicle

    let private updateVehicleMovement minutes world transport =
        let vehicles =
            transport.Vehicles
            |> Map.map (fun _ vehicle ->
                match vehicle.Status with
                | VehicleCompleted
                | VehicleParked
                | VehicleCanceled
                | VehicleFailed -> vehicle
                | _ -> advanceVehicle minutes world transport vehicle)

        { transport with Vehicles = vehicles }

    let private updateTripsAndEvents _ world =
        let completed = completeArrivedTrips world

        let tripsAfterCompletions =
            (world.Transport.Trips, completed)
            ||> List.fold (fun trips (tripId, trip, _) -> Map.add tripId trip trips)

        let demand =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (simTransportDemand { world with Transport = { world.Transport with Trips = tripsAfterCompletions } })
            |> Seq.toList

        let trips =
            (tripsAfterCompletions, demand)
            ||> List.fold (fun trips (trip, _, _, _) -> Map.add trip.Id trip trips)

        let vehicles =
            (world.Transport.Vehicles, demand)
            ||> List.fold (fun vehicles (_, vehicle, _, _) -> Map.add vehicle.Id vehicle vehicles)

        let lateStartEvents (trip: TransportTrip) (route: TransportRoute) =
            match trip.DeadlineMinute with
            | Some deadline ->
                let delay = max 0 (normalizeMinute (world.MinuteOfDay + route.ExpectedMinutes) - deadline)

                if delay > trip.ToleranceForDelayMinutes then
                    [ TripDelayed(trip.Id, delay)
                      match trip.PersonId with
                      | Some simId -> ArrivedLate(simId, trip.Purpose, delay)
                      | None -> () ]
                else
                    []
            | None -> []

        let startedEvents =
            demand
            |> List.collect (fun (trip, vehicle, route, _) ->
                [ TripPlanned trip.Id
                  TripStarted trip.Id
                  ModeChosen(trip.Id, route.Mode)
                  RouteChosen(trip.Id, route.Id) ]
                @ parkingEvents world trip route
                @ laneBehaviorEvents world vehicle route
                @ lateStartEvents trip route)

        let completedEvents = completed |> List.collect (fun (_, _, events) -> events)

        { world.Transport with
            Trips = trips
            Vehicles = vehicles
            RecentEvents = completedEvents @ startedEvents }

    let private updateLaneState world transport =
        let demandBySegment =
            transport.Vehicles
            |> Map.toSeq
            |> Seq.choose (fun (_, vehicle) ->
                match vehicle.Status, vehicle.CurrentPosition with
                | VehicleMoving, OnRoadSegment(segmentId, _, _)
                | VehicleQueued, OnRoadSegment(segmentId, _, _)
                | VehicleWaitingAtIntersection, OnRoadSegment(segmentId, _, _) -> Some segmentId
                | _ -> None)
            |> Seq.countBy id
            |> Map.ofSeq

        let segmentById = world.Map.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList

        let congestion =
            world.Map.RoadSegments
            |> List.map (fun segment ->
                let demand = demandBySegment |> Map.tryFind segment.Id |> Option.defaultValue 0 |> float
                let capacity = max 1.0 (float segment.CapacityPerMinute * 15.0)
                segment.Id, clamp01 (demand / capacity + if segment.UnderConstruction then 0.25 else 0.0))
            |> Map.ofList

        let lanes =
            transport.Lanes
            |> Map.map (fun _ lane ->
                let segment = Map.tryFind lane.SegmentId segmentById
                let c = congestion |> Map.tryFind lane.SegmentId |> Option.defaultValue 0.0
                let baseSpeed = segment |> Option.map _.SpeedKph |> Option.defaultValue lane.CurrentSpeedKph
                let speed = baseSpeed * (1.0 - min 0.80 (c * 0.65))

                { lane with
                    CurrentDensity = c
                    CurrentSpeedKph = max 3.0 speed
                    QueueLength = int (c * 20.0)
                    Blocked = c > 0.95 })

        { transport with
            Lanes = lanes
            SegmentCongestion = congestion }

    let private updateParkingState transport =
        let activeCarArrivals =
            transport.Trips
            |> Map.toSeq
            |> Seq.choose (fun (_, trip) ->
                match trip.Status, trip.CurrentRoute, locationPlace trip.Destination with
                | InProgress, Some route, Some destination when route.RequiresParking -> Some destination
                | _ -> None)
            |> Seq.countBy id
            |> Map.ofSeq

        let parking =
            transport.ParkingZones
            |> Map.map (fun _ zone ->
                let added =
                    zone.NearPlace
                    |> Option.bind (fun placeId -> activeCarArrivals |> Map.tryFind placeId)
                    |> Option.defaultValue 0

                { zone with
                    Occupied = clampInt 0 zone.Capacity (zone.Occupied + added)
                    AverageSearchMinutes = clampInt 1 45 (zone.AverageSearchMinutes + if added > 0 && zone.Occupied > zone.Capacity * 8 / 10 then 2 else 0) })

        { transport with ParkingZones = parking }

    let private updateAccessMetrics world transport =
        let averageCongestion =
            transport.SegmentCongestion
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.append [ 0.0 ]
            |> Seq.average

        let parkingPressure =
            transport.ParkingZones
            |> Map.toSeq
            |> Seq.map (fun (_, zone) -> float zone.Occupied / max 1.0 (float zone.Capacity))
            |> Seq.append [ 0.0 ]
            |> Seq.average

        let transitReliability =
            transport.TransitRoutes
            |> Map.toSeq
            |> Seq.map (fun (_, route) -> route.Reliability * (1.0 - route.Crowding * 0.25))
            |> Seq.append [ 0.65 ]
            |> Seq.average
            |> clamp01

        let accessByNeighborhood =
            world.Neighborhoods
            |> Map.map (fun _ neighborhood ->
                let congestionPenalty = averageCongestion * 0.30
                let parkingPenalty = parkingPressure * 0.12
                let walkSafety = clamp01 (neighborhood.Walkability * 0.70 + neighborhood.Safety * 0.30)
                let bikeSafety = clamp01 (walkSafety * 0.55 + (1.0 - neighborhood.Pollution) * 0.20 + neighborhood.TransitAccess * 0.15)
                let job = clamp01 (neighborhood.EmploymentAccess * 0.55 + neighborhood.TransitAccess * 0.30 + (1.0 - congestionPenalty) * 0.15)
                let school = clamp01 (neighborhood.SchoolAccess * 0.65 + walkSafety * 0.25 + transitReliability * 0.10)
                let food = clamp01 (neighborhood.Walkability * 0.50 + neighborhood.TransitAccess * 0.25 + (1.0 - parkingPenalty) * 0.25)

                { JobAccess = job
                  SchoolAccess = school
                  HealthcareAccess = clamp01 (neighborhood.HealthAccess * 0.70 + transitReliability * 0.20 + walkSafety * 0.10)
                  FoodAccess = food
                  SocialAccess = clamp01 (neighborhood.SocialCohesion * 0.35 + walkSafety * 0.35 + transitReliability * 0.30)
                  EmergencyAccess = clamp01 (neighborhood.Safety * 0.45 + (1.0 - averageCongestion) * 0.45 + neighborhood.ServiceQuality * 0.10)
                  FreightAccess = clamp01 (neighborhood.EmploymentAccess * 0.45 + (1.0 - averageCongestion) * 0.45 + (1.0 - neighborhood.Pollution) * 0.10)
                  ParkingAccess = clamp01 (1.0 - parkingPressure)
                  TransitReliability = transitReliability
                  WalkSafety = walkSafety
                  BikeSafety = bikeSafety
                  OpportunityAccess = clamp01 ((job + school + food + transitReliability + walkSafety) / 5.0) })

        let lateArrivals =
            transport.RecentEvents
            |> List.sumBy (function ArrivedLate _ -> 1 | _ -> 0)

        let metrics =
            { AverageCongestion = averageCongestion
              AverageTravelReliability = transitReliability * 0.45 + (1.0 - averageCongestion) * 0.55
              AverageParkingPressure = parkingPressure
              TransitTrust = transitReliability
              FreightReliability = clamp01 (1.0 - averageCongestion * 0.60)
              EmergencyResponseRisk = clamp01 (averageCongestion * 0.65 + (1.0 - transitReliability) * 0.10)
              LateArrivalsToday = transport.Metrics.LateArrivalsToday + lateArrivals
              FailedLaneChangesToday = transport.Metrics.FailedLaneChangesToday + (transport.RecentEvents |> List.sumBy (function LaneChangeFailed _ -> 1 | _ -> 0))
              MissedTransfersToday = transport.Metrics.MissedTransfersToday + (transport.RecentEvents |> List.sumBy (function MissedTransfer _ -> 1 | _ -> 0))
              ParkingFailuresToday = transport.Metrics.ParkingFailuresToday + (transport.RecentEvents |> List.sumBy (function ParkingFailed _ -> 1 | _ -> 0)) }

        { transport with
            AccessByNeighborhood = accessByNeighborhood
            Metrics = metrics }

    let private applyAccessFeedback world transport =
        let neighborhoods =
            world.Neighborhoods
            |> Map.map (fun neighborhoodId neighborhood ->
                match Map.tryFind neighborhoodId transport.AccessByNeighborhood with
                | None -> neighborhood
                | Some access ->
                    { neighborhood with
                        TransitAccess = clamp01 (neighborhood.TransitAccess * 0.80 + access.TransitReliability * 0.20)
                        EmploymentAccess = clamp01 (neighborhood.EmploymentAccess * 0.82 + access.JobAccess * 0.18)
                        Walkability = clamp01 (neighborhood.Walkability * 0.88 + access.WalkSafety * 0.12)
                        RentPressure = clamp01 (neighborhood.RentPressure + max 0.0 (access.OpportunityAccess - 0.62) * 0.010)
                        Pollution = clamp01 (neighborhood.Pollution + transport.Metrics.AverageCongestion * 0.006) })

        let households =
            world.Households
            |> Map.map (fun _ household ->
                let neighborhoodAccess =
                    neighborhoodForHousehold world household.Id
                    |> Option.bind (fun neighborhoodId -> Map.tryFind neighborhoodId transport.AccessByNeighborhood)

                match neighborhoodAccess with
                | None -> household
                | Some access ->
                    { household with
                        TransportationAccess = clamp01 (household.TransportationAccess * 0.92 + access.OpportunityAccess * 0.08)
                        Stability = clamp01 (household.Stability - max 0.0 (0.45 - access.OpportunityAccess) * 0.015) })

        let indicators =
            { world.City.Indicators with
                Traffic = clamp01 (world.City.Indicators.Traffic * 0.45 + transport.Metrics.AverageCongestion * 0.55)
                Pollution = clamp01 (world.City.Indicators.Pollution + transport.Metrics.AverageCongestion * 0.020) }

        { world with
            Transport = transport
            Neighborhoods = neighborhoods
            Households = households
            City = { world.City with Indicators = indicators } }

    let tick minutes world =
        let transport =
            updateTripsAndEvents minutes world
            |> updateVehicleMovement minutes world
            |> updateLaneState world
            |> updateParkingState
            |> updateAccessMetrics world

        let routeCalculations =
            transport.RecentEvents
            |> List.sumBy (function RouteChosen _ -> 1 | _ -> 0)

        let world =
            { world with
                PerformanceDiagnostics =
                    { world.PerformanceDiagnostics with
                        RouteCalculations = world.PerformanceDiagnostics.RouteCalculations + routeCalculations
                        CacheMisses = world.PerformanceDiagnostics.CacheMisses + routeCalculations
                        TripsProcessed = transport.Trips.Count } }

        applyAccessFeedback world transport

module TrafficVisualization =
    open Simulation.Domain
    open Simulation.Measures

    let private renderPosition (coordinates: Coordinates) : VehicleRenderPosition =
        { RenderX = coordinates.X; RenderY = coordinates.Y; RenderZ = None }

    let private segmentById (world: World) =
        world.Map.RoadSegments
        |> List.map (fun segment -> segment.Id, segment)
        |> Map.ofList

    let private activeVisualStatus =
        function
        | VehicleMoving -> Moving
        | VehicleWaitingAtIntersection -> WaitingAtIntersectionVisual
        | VehicleQueued -> QueuedVisual
        | VehicleParked -> ParkedVisual
        | VehicleCompleted -> CompletedVisual
        | VehicleNotStarted -> StoppedVisual
        | VehicleCanceled
        | VehicleFailed -> HiddenVisual

    let private positionOnSegment (world: World) (route: TransportRoute option) routeIndex segmentId progress =
        let segmentMap = segmentById world

        match Map.tryFind segmentId segmentMap with
        | None -> { RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None
        | Some segment ->
            let fromNode, toNode =
                match route, routeIndex with
                | Some route, Some index ->
                    match route.NodePath |> List.tryItem index, route.NodePath |> List.tryItem (index + 1) with
                    | Some fromNode, Some toNode -> fromNode, toNode
                    | _ -> segment.From, segment.To
                | _ -> segment.From, segment.To

            match Map.tryFind fromNode world.Map.RoadNodes, Map.tryFind toNode world.Map.RoadNodes with
            | Some fromNode, Some toNode ->
                let p = clamp01 progress
                let x = fromNode.Position.X + (toNode.Position.X - fromNode.Position.X) * p
                let y = fromNode.Position.Y + (toNode.Position.Y - fromNode.Position.Y) * p
                let heading = Math.Atan2(toNode.Position.Y - fromNode.Position.Y, toNode.Position.X - fromNode.Position.X)
                { RenderX = x; RenderY = y; RenderZ = None }, Some heading
            | _ -> { RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None

    let private positionFromVehiclePosition (world: World) (route: TransportRoute option) routeIndex =
        function
        | OnRoadSegment(segmentId, _, progress) ->
            positionOnSegment world route routeIndex segmentId progress
        | WaitingAtIntersection(nodeId, _) ->
            world.Map.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map (fun node -> renderPosition node.Position, None)
            |> Option.defaultValue ({ RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None)
        | ParkedAt(_, Some placeId) ->
            world.Map.Places
            |> Map.tryFind placeId
            |> Option.map (fun place -> renderPosition place.Position, None)
            |> Option.defaultValue ({ RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None)
        | AtStop stopId ->
            world.Transport.TransitStops
            |> Map.tryFind stopId
            |> Option.map (fun stop -> renderPosition stop.Position, None)
            |> Option.defaultValue ({ RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None)
        | OffNetwork
        | CompletedTripPosition -> { RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None
        | ParkedAt(_, None) -> { RenderX = 0.0; RenderY = 0.0; RenderZ = None }, None

    let getVehicleView (world: World) (vehicleId: VehicleId) : VehicleView option =
        world.Transport.Vehicles
        |> Map.tryFind vehicleId
        |> Option.map (fun vehicle ->
            let trip = world.Transport.Trips |> Map.tryFind vehicle.Trip
            let route = trip |> Option.bind _.CurrentRoute
            let position, heading = positionFromVehiclePosition world route vehicle.CurrentRouteIndex vehicle.CurrentPosition
            let previous =
                vehicle.PreviousPosition
                |> Option.map (fun prior -> positionFromVehiclePosition world route vehicle.CurrentRouteIndex prior |> fst)

            let segmentId, laneId, intersectionId, progress =
                match vehicle.CurrentPosition with
                | OnRoadSegment(segmentId, laneId, progress) -> Some segmentId, laneId, None, Some progress
                | WaitingAtIntersection(intersectionId, laneId) -> None, laneId, Some intersectionId, None
                | _ -> None, None, None, None

            let view: VehicleView =
                { VehicleId = vehicle.Id
                  TripId = Some vehicle.Trip
                  Mode = vehicle.Mode
                  SegmentId = segmentId
                  LaneId = laneId
                  IntersectionId = intersectionId
                  Position = position
                  PreviousPosition = previous
                  ProgressAlongSegment = progress
                  HeadingRadians = heading
                  SpeedKph = vehicle.CurrentSpeedKph
                  Status = activeVisualStatus vehicle.Status
                  RouteIndex = vehicle.CurrentRouteIndex
                  Occupancy = vehicle.Occupants }

            view)

    let private allVehicleViews (world: World) : VehicleView list =
        world.Transport.Vehicles
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.choose (fun (vehicleId, vehicle) ->
            match vehicle.Status with
            | VehicleCompleted
            | VehicleCanceled
            | VehicleFailed -> None
            | _ -> getVehicleView world vehicleId)
        |> Seq.toList

    let getVehiclesOnRoadSegment (world: World) segmentId : VehicleView list =
        allVehicleViews world
        |> List.filter (fun vehicle -> vehicle.SegmentId = Some segmentId)

    let getVehiclesAtIntersection (world: World) intersectionId : VehicleView list =
        allVehicleViews world
        |> List.filter (fun vehicle -> vehicle.IntersectionId = Some intersectionId)

    let private roadSegmentView (world: World) (vehicles: VehicleView list) (segment: RoadSegment) : RoadSegmentTrafficView =
        let onSegment = vehicles |> List.filter (fun vehicle -> vehicle.SegmentId = Some segment.Id)
        let averageSpeed =
            onSegment
            |> List.map _.SpeedKph
            |> List.append [ 0.0 ]
            |> List.average

        let queueLength =
            world.Transport.Lanes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun lane -> lane.SegmentId = segment.Id)
            |> Seq.sumBy _.QueueLength

        let startPosition =
            world.Map.RoadNodes
            |> Map.tryFind segment.From
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let endPosition =
            world.Map.RoadNodes
            |> Map.tryFind segment.To
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let view: RoadSegmentTrafficView =
            { SegmentId = segment.Id
              StartPosition = startPosition
              EndPosition = endPosition
              RoadClass = segment.RoadClass
              LaneCount = segment.LaneIds.Length
              IsTwoWay = segment.IsTwoWay
              ActiveVehicleCount = onSegment.Length
              AverageSpeedKph = averageSpeed
              Congestion = world.Transport.SegmentCongestion |> Map.tryFind segment.Id |> Option.defaultValue 0.0
              QueueLength = queueLength
              IsClosed = segment.UnderConstruction || not segment.CurrentIncidents.IsEmpty }

        view

    let private intersectionView (world: World) (vehicles: VehicleView list) nodeId (intersection: Intersection) : IntersectionTrafficView =
        let waiting = vehicles |> List.filter (fun vehicle -> vehicle.IntersectionId = Some nodeId)
        let averageDelay =
            waiting
            |> List.choose (fun vehicle -> world.Transport.Vehicles |> Map.tryFind vehicle.VehicleId)
            |> List.map (fun vehicle -> float vehicle.DelayMinutes * 60.0)
            |> List.append [ 0.0 ]
            |> List.average

        let position =
            world.Map.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let view: IntersectionTrafficView =
            { IntersectionId = nodeId
              Position = position
              WaitingVehicleCount = waiting.Length
              AverageDelaySeconds = averageDelay
              ControlType = intersection.Control
              CurrentPhase = if intersection.SignalPhases.IsEmpty then None else Some 0 }

        view

    let private frameMetrics (world: World) (vehicles: VehicleView list) : TrafficFrameMetrics =
        let moving = vehicles |> List.filter (fun vehicle -> vehicle.Status = Moving)
        let waiting = vehicles |> List.filter (fun vehicle -> vehicle.Status = WaitingAtIntersectionVisual || vehicle.Status = QueuedVisual)
        let parked = vehicles |> List.filter (fun vehicle -> vehicle.Status = ParkedVisual)
        let completed =
            world.Transport.Vehicles
            |> Map.toSeq
            |> Seq.sumBy (fun (_, vehicle) -> if vehicle.Status = VehicleCompleted then 1 else 0)

        { ActiveVehicleCount = vehicles.Length
          MovingVehicleCount = moving.Length
          WaitingVehicleCount = waiting.Length
          ParkedVehicleCount = parked.Length
          CompletedVehicleCount = completed
          AverageVehicleSpeedKph = vehicles |> List.map _.SpeedKph |> List.append [ 0.0 ] |> List.average
          AverageCongestion = world.Transport.Metrics.AverageCongestion }

    let getTrafficFrame (world: World) : TrafficFrame =
        let vehicles = allVehicleViews world

        let frame: TrafficFrame =
            { Tick = TickId world.Meta.Tick
              SimTime = { Day = world.Day; MinuteOfDay = world.MinuteOfDay }
              Vehicles = vehicles
              RoadSegments =
                world.Map.RoadSegments
                |> List.sortBy _.Id
                |> List.map (roadSegmentView world vehicles)
              Intersections =
                world.Transport.Intersections
                |> Map.toSeq
                |> Seq.sortBy fst
                |> Seq.map (fun (nodeId, intersection) -> intersectionView world vehicles nodeId intersection)
                |> Seq.toList
              TransitVehicles = vehicles |> List.filter (fun vehicle -> vehicle.Mode = Bus || vehicle.Mode = Tram || vehicle.Mode = Metro || vehicle.Mode = RegionalRail)
              Events = []
              Metrics = frameMetrics world vehicles }

        frame

    let getRenderableRoute (world: World) (tripId: TransportTripId) : RenderableRoute option =
        world.Transport.Trips
        |> Map.tryFind tripId
        |> Option.bind (fun trip ->
            trip.CurrentRoute
            |> Option.map (fun route ->
                let segments =
                    route.SegmentIds
                    |> List.mapi (fun index segmentId ->
                        let fromPosition, toPosition =
                            match route.NodePath |> List.tryItem index, route.NodePath |> List.tryItem (index + 1) with
                            | Some fromNode, Some toNode ->
                                let fromPos =
                                    world.Map.RoadNodes
                                    |> Map.tryFind fromNode
                                    |> Option.map (fun node -> renderPosition node.Position)
                                    |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

                                let toPos =
                                    world.Map.RoadNodes
                                    |> Map.tryFind toNode
                                    |> Option.map (fun node -> renderPosition node.Position)
                                    |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

                                fromPos, toPos
                            | _ ->
                                let segmentMap = segmentById world
                                match Map.tryFind segmentId segmentMap with
                                | Some segment ->
                                    let fromPos = world.Map.RoadNodes |> Map.tryFind segment.From |> Option.map (fun node -> renderPosition node.Position) |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }
                                    let toPos = world.Map.RoadNodes |> Map.tryFind segment.To |> Option.map (fun node -> renderPosition node.Position) |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }
                                    fromPos, toPos
                                | None -> { RenderX = 0.0; RenderY = 0.0; RenderZ = None }, { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

                        let segmentView: RenderableRouteSegment =
                            { SegmentId = segmentId
                              FromPosition = fromPosition
                              ToPosition = toPosition
                              ExpectedTravelMinutes = route.SegmentTravelMinutes |> List.tryItem index |> Option.defaultValue 0
                              ExpectedIntersectionDelayMinutes = route.IntersectionDelayMinutes |> List.tryItem index |> Option.defaultValue 0 }

                        segmentView)

                let renderable: RenderableRoute =
                    { TripId = tripId
                      RouteId = route.Id
                      Mode = route.Mode
                      Segments = segments
                      ExpectedMinutes = route.ExpectedMinutes }

                renderable))

    let diffTrafficFrames (previous: TrafficFrame) (current: TrafficFrame) : TrafficFrameDiff =
        let previousVehicles = previous.Vehicles |> List.map (fun vehicle -> vehicle.VehicleId, vehicle) |> Map.ofList
        let currentVehicles = current.Vehicles |> List.map (fun vehicle -> vehicle.VehicleId, vehicle) |> Map.ofList

        let added =
            current.Vehicles
            |> List.filter (fun vehicle -> not (Map.containsKey vehicle.VehicleId previousVehicles))

        let updated =
            current.Vehicles
            |> List.filter (fun vehicle ->
                previousVehicles
                |> Map.tryFind vehicle.VehicleId
                |> Option.exists (fun prev -> prev <> vehicle))

        let removed =
            previous.Vehicles
            |> List.filter (fun vehicle -> not (Map.containsKey vehicle.VehicleId currentVehicles))
            |> List.map _.VehicleId

        let previousRoads = previous.RoadSegments |> List.map (fun road -> road.SegmentId, road) |> Map.ofList
        let changedRoads =
            current.RoadSegments
            |> List.filter (fun road -> previousRoads |> Map.tryFind road.SegmentId |> Option.exists (fun prev -> prev <> road))

        let previousIntersections = previous.Intersections |> List.map (fun intersection -> intersection.IntersectionId, intersection) |> Map.ofList
        let changedIntersections =
            current.Intersections
            |> List.filter (fun intersection -> previousIntersections |> Map.tryFind intersection.IntersectionId |> Option.exists (fun prev -> prev <> intersection))

        let vehicleEvents =
            [ for vehicle in added do
                  match vehicle.SegmentId with
                  | Some segmentId -> VehicleEnteredSegment(vehicle.VehicleId, segmentId)
                  | None -> ()

              for vehicle in updated do
                  match Map.tryFind vehicle.VehicleId previousVehicles with
                  | Some prior ->
                      match prior.SegmentId, vehicle.SegmentId with
                      | Some priorSegment, Some segment when priorSegment <> segment ->
                          VehicleLeftSegment(vehicle.VehicleId, priorSegment)
                          VehicleEnteredSegment(vehicle.VehicleId, segment)
                      | None, Some segment -> VehicleEnteredSegment(vehicle.VehicleId, segment)
                      | Some priorSegment, None -> VehicleLeftSegment(vehicle.VehicleId, priorSegment)
                      | _ -> ()

                      if prior.IntersectionId <> vehicle.IntersectionId then
                          match vehicle.IntersectionId with
                          | Some intersectionId -> VehicleStoppedAtIntersection(vehicle.VehicleId, intersectionId)
                          | None when prior.Status = WaitingAtIntersectionVisual || prior.Status = QueuedVisual -> VehicleStartedMoving vehicle.VehicleId
                          | _ -> ()
                  | None -> ()

              for vehicleId in removed do
                  match Map.tryFind vehicleId previousVehicles |> Option.bind _.TripId with
                  | Some tripId -> VehicleCompletedTrip(vehicleId, tripId)
                  | None -> () ]

        { Tick = current.Tick
          AddedVehicles = added
          UpdatedVehicles = updated
          RemovedVehicles = removed
          ChangedRoadSegments = changedRoads
          ChangedIntersections = changedIntersections
          Events =
            vehicleEvents
            @ (changedRoads |> List.map (fun road -> RoadTrafficStateChanged road.SegmentId))
            @ (changedIntersections |> List.map (fun intersection -> IntersectionStateChanged intersection.IntersectionId)) }

