namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain

module TransportAndDecisionTests =
    let private morningWorld () =
        TestWorld.create () |> TestWorld.runTicks 15 12

    let private guid suffix =
        Guid.Parse(sprintf "20000000-0000-0000-0000-%012x" suffix)

    let private segmentLength world fromNode toNode =
        MapGraph.distanceMeters world.Map world.Map.RoadNodes[fromNode].Position world.Map.RoadNodes[toNode].Position

    let private addParallelRoad suffix name roadClass speedKph capacity fromNode toNode world =
        let segmentId = RoadSegmentId(guid suffix)
        let forwardLaneId = LaneId(guid (suffix + 1))
        let reverseLaneId = LaneId(guid (suffix + 2))
        let length = segmentLength world fromNode toNode
        let allowedGeneral = [ PrivateCar; TaxiOrRideshare; Bus; ServiceVehicle; DeliveryVehicle; EmergencyVehicle; SchoolBus ] |> Set.ofList

        let lane laneId direction =
            { Id = laneId
              SegmentId = segmentId
              Direction = direction
              LaneType = General
              AllowedModes = allowedGeneral
              PermittedMovements = [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList
              LengthMeters = length
              CapacityPerHour = float capacity * 60.0
              CurrentDensity = 0.0
              CurrentSpeedKph = speedKph
              QueueLength = 0
              Blocked = false }

        let lanes = [ lane forwardLaneId Forward; lane reverseLaneId Reverse ]

        let segment =
            { Id = segmentId
              Name = name
              From = fromNode
              To = toNode
              LengthMeters = length
              SpeedKph = speedKph
              IsTwoWay = true
              CapacityPerMinute = capacity
              RoadClass = roadClass
              LaneIds = lanes |> List.map _.Id
              ParkingRules = []
              TransitLaneIds = []
              BikeFacility = NoBikeFacility
              SidewalkQuality = 0.35
              Grade = 0.0
              SurfaceCondition = 1.0
              Toll = None
              Restrictions = Set.empty
              CurrentIncidents = Set.empty
              UnderConstruction = false
              WeatherImpact = 0.0
              NoiseOutput = 0.20
              PollutionOutput = 0.18 }

        { world with
            Map = { world.Map with RoadSegments = segment :: world.Map.RoadSegments }
            Transport =
                { world.Transport with
                    Lanes = (world.Transport.Lanes, lanes) ||> List.fold (fun map lane -> Map.add lane.Id lane map)
                    SegmentCongestion = Map.add segment.Id 0.0 world.Transport.SegmentCongestion } },
        segment

    let private addNode suffix x y world =
        let nodeId = RoadNodeId(guid suffix)
        { world with
            Map =
                { world.Map with
                    RoadNodes = Map.add nodeId { Id = nodeId; Position = { X = x; Y = y } } world.Map.RoadNodes } },
        nodeId

    let private laneFor segmentId laneId direction speedKph capacity length =
        { Id = laneId
          SegmentId = segmentId
          Direction = direction
          LaneType = General
          AllowedModes = [ PrivateCar; TaxiOrRideshare; Bus; ServiceVehicle; DeliveryVehicle; EmergencyVehicle; SchoolBus ] |> Set.ofList
          PermittedMovements = [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList
          LengthMeters = length
          CapacityPerHour = float capacity * 60.0
          CurrentDensity = 0.0
          CurrentSpeedKph = speedKph
          QueueLength = 0
          Blocked = false }

    let private addSegment suffix name speedKph capacity roadClass fromNode toNode world =
        let segmentId = RoadSegmentId(guid suffix)
        let forwardLaneId = LaneId(guid (suffix + 1))
        let reverseLaneId = LaneId(guid (suffix + 2))
        let length = segmentLength world fromNode toNode
        let lanes = [ laneFor segmentId forwardLaneId Forward speedKph capacity length; laneFor segmentId reverseLaneId Reverse speedKph capacity length ]

        let segment =
            { Id = segmentId
              Name = name
              From = fromNode
              To = toNode
              LengthMeters = length
              SpeedKph = speedKph
              IsTwoWay = true
              CapacityPerMinute = capacity
              RoadClass = roadClass
              LaneIds = lanes |> List.map _.Id
              ParkingRules = []
              TransitLaneIds = []
              BikeFacility = NoBikeFacility
              SidewalkQuality = 0.35
              Grade = 0.0
              SurfaceCondition = 1.0
              Toll = None
              Restrictions = Set.empty
              CurrentIncidents = Set.empty
              UnderConstruction = false
              WeatherImpact = 0.0
              NoiseOutput = 0.20
              PollutionOutput = 0.18 }

        { world with
            Map = { world.Map with RoadSegments = segment :: world.Map.RoadSegments }
            Transport =
                { world.Transport with
                    Lanes = (world.Transport.Lanes, lanes) ||> List.fold (fun map lane -> Map.add lane.Id lane map)
                    SegmentCongestion = Map.add segment.Id 0.0 world.Transport.SegmentCongestion } },
        segment

    let private addIntersection nodeId control queueRisk mergeDifficulty world =
        let touching =
            world.Map.RoadSegments
            |> List.filter (fun segment -> segment.From = nodeId || segment.To = nodeId)
            |> List.collect _.LaneIds
            |> Set.ofList

        let phases =
            [ { Kind = ThroughPhase; DurationSeconds = 30; Movements = [ MoveThrough; MoveRight ] |> Set.ofList }
              { Kind = ProtectedLeftPhase; DurationSeconds = 12; Movements = [ MoveLeft ] |> Set.ofList }
              { Kind = TransitPriorityPhase; DurationSeconds = 8; Movements = [ MoveThrough; MoveRight ] |> Set.ofList } ]

        let intersection =
            { Node = nodeId
              IncomingLanes = touching
              OutgoingLanes = touching
              PermittedMovements = touching |> Seq.map (fun laneId -> laneId, ([ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList)) |> Map.ofSeq
              Control = control
              SignalPhases = phases
              CrosswalkQuality = 0.65
              BikeCrossingQuality = 0.55
              CapacityPerMinute = 30
              QueueSpillbackRisk = queueRisk
              MergeDifficulty = mergeDifficulty
              VisibilitySafety = 0.55
              IncidentRisk = 0.10 }

        { world with Transport = { world.Transport with Intersections = Map.add nodeId intersection world.Transport.Intersections } }

    let private privateCarTripWorld origin destination world =
        let simId, sim =
            world.Sims
            |> Map.toSeq
            |> Seq.find (fun (_, sim) -> sim.LifeStage <> Child)

        let household = world.Households[sim.Household]
        let sim =
            { sim with
                Location =
                    InTransit
                        { Origin = origin
                          Destination = destination
                          Purpose = ToWork
                          RemainingMinutes = 90
                          TotalMinutes = 90 } }

        { world with
            Sims = Map.add simId sim world.Sims
            Households = Map.add sim.Household { household with TransportationAccess = 1.0; Assets = 25_000m } world.Households
            Transport =
                { world.Transport with
                    TransitRoutes = Map.empty
                    Trips = Map.empty
                    Vehicles = Map.empty
                    RecentEvents = [] } }

    let private firstCurrentRoute world =
        world.Transport.Trips
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.choose _.CurrentRoute
        |> Seq.head

    let private firstVehicle world =
        world.Transport.Vehicles |> Map.toSeq |> Seq.map snd |> Seq.head

    let private withFirstVehicle update world =
        let vehicle = firstVehicle world
        { world with Transport = { world.Transport with Vehicles = Map.add vehicle.Id (update vehicle) world.Transport.Vehicles } }

    let private withFirstTripRoute update world =
        let trip =
            world.Transport.Trips
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.find (fun trip -> trip.CurrentRoute.IsSome)

        let route = update trip.CurrentRoute.Value
        let trip = { trip with CurrentRoute = Some route; PlannedRoute = Some route }
        { world with Transport = { world.Transport with Trips = Map.add trip.Id trip world.Transport.Trips } }

    [<Fact>]
    let ``ModeChoiceIncludesDecisionReasons`` () =
        let world = morningWorld ()
        let chosenTrips =
            world.Transport.Trips
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun trip -> trip.ChosenMode.IsSome)
            |> Seq.toList

        Assert.NotEmpty(chosenTrips)
        Assert.All(chosenTrips, fun trip -> Assert.NotEmpty(trip.ModeChoiceReasons))
        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``ParkingScarcityChangesModeChoice`` () =
        let baseWorld = TestWorld.create ()
        let initial =
            { baseWorld with
                Transport =
                    { baseWorld.Transport with
                        ParkingZones =
                            baseWorld.Transport.ParkingZones
                            |> Map.map (fun _ zone ->
                                { zone with
                                    Occupied = zone.Capacity
                                    AverageSearchMinutes = 35
                                    PricePerHour = 18m }) } }

        let world = initial |> TestWorld.runTicks 15 12
        let workTrips =
            world.Transport.Trips
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun trip -> trip.Purpose = WorkTrip)
            |> Seq.toList

        Assert.NotEmpty(workTrips)
        Assert.Contains(workTrips, fun trip ->
            trip.ChosenMode <> Some PrivateCar
            || List.contains ParkingUnavailable trip.ModeChoiceReasons
            || List.contains ParkingTooExpensive trip.ModeChoiceReasons)

        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``SpeedLimitAffectsRouteChoice`` () =
        let world = TestWorld.create ()
        let highway = world.Map.RoadSegments |> List.find (fun segment -> segment.RoadClass = Highway)
        let destination =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 80.0)
            |> fst

        let origin =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let world, express = addParallelRoad 10 "Audit Express Link" Highway 120.0 160 highway.From highway.To world
        let result = world |> privateCarTripWorld origin destination |> Transport.tick 15
        let route = firstCurrentRoute result

        Assert.Contains(express.Id, route.SegmentIds)
        Invariants.checkWorld result |> ignore

    [<Fact>]
    let ``CongestionCanMakeHighwaySlowerThanLocalRoute`` () =
        let world = TestWorld.create ()
        let highway = world.Map.RoadSegments |> List.find (fun segment -> segment.RoadClass = Highway)
        let destination =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 80.0)
            |> fst

        let origin =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let world, localDetour = addParallelRoad 30 "Audit Local Detour" LocalStreet 35.0 80 highway.From highway.To world
        let congested =
            { world with
                Transport = { world.Transport with SegmentCongestion = Map.add highway.Id 1.0 world.Transport.SegmentCongestion } }

        let result = congested |> privateCarTripWorld origin destination |> Transport.tick 15
        let route = firstCurrentRoute result

        Assert.Contains(localDetour.Id, route.SegmentIds)
        Assert.DoesNotContain(highway.Id, route.SegmentIds)
        Invariants.checkWorld result |> ignore

    [<Fact>]
    let ``SignalizedIntersectionAddsDelay`` () =
        let world = TestWorld.create ()
        let nodeId, intersection =
            world.Transport.Intersections
            |> Map.toSeq
            |> Seq.find (fun (_, intersection) -> match intersection.Control with Signalized _ -> true | _ -> false)

        let nextSegment = world.Map.RoadSegments |> List.find (fun segment -> segment.From = nodeId || segment.To = nodeId)
        let signalDelay = Transport.intersectionDelayMinutes world PrivateCar nodeId None nextSegment
        let uncontrolled =
            { world with
                Transport =
                    { world.Transport with
                        Intersections = Map.add nodeId { intersection with Control = Uncontrolled; QueueSpillbackRisk = 0.0; MergeDifficulty = 0.0 } world.Transport.Intersections } }

        let uncontrolledDelay = Transport.intersectionDelayMinutes uncontrolled PrivateCar nodeId None nextSegment

        Assert.True(signalDelay > uncontrolledDelay)

    [<Fact>]
    let ``StopSignAddsDelay`` () =
        let world = TestWorld.create ()
        let nodeId, _ =
            world.Transport.Intersections
            |> Map.toSeq
            |> Seq.find (fun (_, intersection) -> intersection.Control = StopSign)

        let nextSegment = world.Map.RoadSegments |> List.find (fun segment -> segment.From = nodeId || segment.To = nodeId)
        let delay = Transport.intersectionDelayMinutes world PrivateCar nodeId None nextSegment

        Assert.True(delay > 0)

    [<Fact>]
    let ``TurnMovementAffectsIntersectionDelay`` () =
        let world, center = TestWorld.create () |> addNode 300 0.0 0.0
        let world, west = world |> addNode 301 -1.0 0.0
        let world, east = world |> addNode 302 1.0 0.0
        let world, north = world |> addNode 303 0.0 1.0
        let world, incoming = world |> addSegment 310 "Incoming" 40.0 60 Collector west center
        let world, straight = world |> addSegment 320 "Straight" 40.0 60 Collector center east
        let world, left = world |> addSegment 330 "Left" 40.0 60 Collector center north
        let world = addIntersection center StopSign 0.0 0.0 world

        let straightDelay = Transport.intersectionDelayMinutes world PrivateCar center (Some incoming) straight
        let leftDelay = Transport.intersectionDelayMinutes world PrivateCar center (Some incoming) left

        Assert.True(leftDelay > straightDelay)

    [<Fact>]
    let ``BusCanBenefitFromTransitPriorityIfModeled`` () =
        let world = TestWorld.create ()
        let nodeId, _ =
            world.Transport.Intersections
            |> Map.toSeq
            |> Seq.find (fun (_, intersection) -> intersection.SignalPhases |> List.exists (fun phase -> phase.Kind = TransitPriorityPhase))

        let nextSegment = world.Map.RoadSegments |> List.find (fun segment -> segment.From = nodeId || segment.To = nodeId)

        Assert.True(Transport.intersectionDelayMinutes world Bus nodeId None nextSegment < Transport.intersectionDelayMinutes world PrivateCar nodeId None nextSegment)

    [<Fact>]
    let ``IntersectionDelayAffectsRouteChoice`` () =
        let baseWorld = TestWorld.create ()
        let officeNode =
            baseWorld.Map.RoadSegments
            |> List.find (fun segment -> segment.Name = "Civic Parkway")
            |> _.To

        let mallNode =
            baseWorld.Map.RoadSegments
            |> List.find (fun segment -> segment.Name = "Regional Connector")
            |> _.To

        let baseWorld, fastMiddle = baseWorld |> addNode 400 60.0 5.0
        let baseWorld, slowerMiddle = baseWorld |> addNode 401 60.0 20.0
        let baseWorld, fastA = baseWorld |> addSegment 410 "Fast signal approach" 75.0 90 Highway officeNode fastMiddle
        let baseWorld, fastB = baseWorld |> addSegment 420 "Fast signal exit" 75.0 90 Highway fastMiddle mallNode
        let baseWorld, slowA = baseWorld |> addSegment 430 "Calm bypass approach" 70.0 90 Collector officeNode slowerMiddle
        let baseWorld, slowB = baseWorld |> addSegment 440 "Calm bypass exit" 70.0 90 Collector slowerMiddle mallNode
        let baseWorld = addIntersection fastMiddle RailroadCrossing 1.0 1.0 baseWorld
        let baseWorld = addIntersection slowerMiddle Uncontrolled 0.0 0.0 baseWorld

        let origin =
            baseWorld.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let destination =
            baseWorld.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 80.0)
            |> fst

        let withoutDelay =
            { baseWorld with
                Transport =
                    { baseWorld.Transport with
                        Intersections =
                            baseWorld.Transport.Intersections
                            |> Map.map (fun _ intersection -> { intersection with Control = Uncontrolled; QueueSpillbackRisk = 0.0; MergeDifficulty = 0.0 }) } }
            |> privateCarTripWorld origin destination
            |> Transport.tick 1
            |> firstCurrentRoute

        let withDelay =
            baseWorld
            |> privateCarTripWorld origin destination
            |> Transport.tick 1
            |> firstCurrentRoute

        Assert.Contains(fastA.Id, withoutDelay.SegmentIds)
        Assert.Contains(fastB.Id, withoutDelay.SegmentIds)
        Assert.Contains(slowA.Id, withDelay.SegmentIds)
        Assert.Contains(slowB.Id, withDelay.SegmentIds)

    [<Fact>]
    let ``SameSeedProducesSameIntersectionRouting`` () =
        let routeText () =
            let world = TestWorld.create ()
            let origin =
                world.Map.Places
                |> Map.toSeq
                |> Seq.find (fun (_, place) -> place.Kind = Workplace)
                |> fst

            let destination =
                world.Map.Places
                |> Map.toSeq
                |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 80.0)
                |> fst

            world |> privateCarTripWorld origin destination |> Transport.tick 1 |> firstCurrentRoute |> sprintf "%A"

        Assert.Equal(routeText (), routeText ())

    [<Fact>]
    let ``TransitDelayReducesTransitTrust`` () =
        let initial = TestWorld.create ()
        let unreliable =
            { initial with
                Transport =
                    { initial.Transport with
                        TransitRoutes =
                            initial.Transport.TransitRoutes
                            |> Map.map (fun _ route ->
                                { route with
                                    Reliability = 0.20
                                    Crowding = 0.90 }) } }

        let world = unreliable |> TestWorld.runTicks 15 12

        Assert.True(world.Transport.Metrics.TransitTrust < initial.Transport.Metrics.TransitTrust)
        Assert.Contains(world.Transport.Trips |> Map.toSeq |> Seq.map snd, fun trip ->
            trip.ChosenMode = Some Bus && List.contains TransitUnreliable trip.ModeChoiceReasons)

        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``TransitTripIncludesWaitTime`` () =
        let initial = TestWorld.create ()
        let world =
            { initial with
                Households =
                    initial.Households
                    |> Map.map (fun _ household -> { household with TransportationAccess = 0.0; Assets = 0m }) }
            |> TestWorld.runTicks 15 12

        let busTrip =
            world.Transport.Trips
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.find (fun trip -> trip.ChosenMode = Some Bus && trip.CurrentRoute.IsSome)

        let route = busTrip.CurrentRoute.Value
        let transit = world.Transport.TransitRoutes[route.TransitRouteId.Value]
        let segmentById = world.Map.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList
        let freeFlowRoadMinutes =
            route.SegmentIds
            |> List.sumBy (fun segmentId ->
                let segment = segmentById[segmentId]
                int (Math.Ceiling((segment.LengthMeters / 1000.0) / segment.SpeedKph * 60.0)))

        let expectedWaitAndDwell = max 1 (transit.HeadwayMinutes / 2) + max 2 transit.Stops.Length

        Assert.True(route.ExpectedMinutes >= freeFlowRoadMinutes + expectedWaitAndDwell)
        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``BusOnlyLaneRejectsPrivateCar`` () =
        let world = TestWorld.create ()
        let busOnlyLanes =
            world.Transport.Lanes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun lane -> lane.LaneType = BusOnly)
            |> Seq.toList

        Assert.NotEmpty(busOnlyLanes)
        Assert.All(busOnlyLanes, fun lane ->
            Assert.False(Set.contains PrivateCar lane.AllowedModes, "Bus-only lane data must reject private cars."))
        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``NoCarHouseholdCannotChoosePrivateCar`` () =
        let initial = TestWorld.create ()
        let carless =
            { initial with
                Households =
                    initial.Households
                    |> Map.map (fun _ household -> { household with TransportationAccess = 0.0; Assets = 0m }) }

        let world = carless |> TestWorld.runTicks 15 12
        let chosenTrips = world.Transport.Trips |> Map.toSeq |> Seq.map snd |> Seq.filter (fun trip -> trip.ChosenMode.IsSome) |> Seq.toList

        Assert.NotEmpty(chosenTrips)
        Assert.DoesNotContain(chosenTrips, fun trip -> trip.ChosenMode = Some PrivateCar)
        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``SameSeedProducesSameTransportEvents`` () =
        let transportEvents world =
            world.Meta.EventLog
            |> List.choose (function TransportEventOccurred(_, transportEvent) -> Some(sprintf "%A" transportEvent) | _ -> None)

        let world1 = TestWorld.create () |> TestWorld.runTicks 15 12
        let world2 = TestWorld.create () |> TestWorld.runTicks 15 12

        Assert.Equal<string list>(transportEvents world1, transportEvents world2)
        Invariants.checkWorld world1 |> ignore

    [<Fact>]
    let ``StartingTripCreatesVehiclePosition`` () =
        let world = morningWorld ()
        let vehicle = firstVehicle world

        match vehicle.CurrentPosition with
        | OnRoadSegment(segmentId, laneId, progress) ->
            Assert.Contains(segmentId, vehicle.Trip |> fun tripId -> world.Transport.Trips[tripId].CurrentRoute.Value.SegmentIds)
            Assert.True(laneId.IsSome)
            Assert.InRange(progress, 0.0, 1.0)
        | WaitingAtIntersection _ -> Assert.True(true)
        | other -> Assert.Fail($"Expected route-derived road position, got {other}.")

    [<Fact>]
    let ``VehicleMovesAlongCurrentRoadSegment`` () =
        let initial = TestWorld.create ()
        let origin =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Residence)
            |> fst

        let destination =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let before = firstVehicle started
        let after = started |> Transport.tick 1 |> firstVehicle

        match before.CurrentPosition, after.CurrentPosition with
        | OnRoadSegment(beforeSegment, _, beforeProgress), OnRoadSegment(afterSegment, _, afterProgress) when beforeSegment = afterSegment ->
            Assert.True(afterProgress > beforeProgress)
        | _ -> Assert.Fail("Expected vehicle to advance on its current road segment.")

    [<Fact>]
    let ``VehicleTransitionsToNextSegment`` () =
        let world = morningWorld ()
        let vehicle = firstVehicle world
        let route = world.Transport.Trips[vehicle.Trip].CurrentRoute.Value
        let zeroDelayRoute = { route with IntersectionDelayMinutes = route.IntersectionDelayMinutes |> List.map (fun _ -> 0) }
        let trip = world.Transport.Trips[vehicle.Trip]

        let prepared =
            { world with
                Transport =
                    { world.Transport with
                        Trips = Map.add vehicle.Trip { trip with CurrentRoute = Some zeroDelayRoute; PlannedRoute = Some zeroDelayRoute } world.Transport.Trips } }
            |> withFirstVehicle (fun vehicle ->
                let firstSegment = zeroDelayRoute.SegmentIds[0]
                { vehicle with
                    CurrentPosition = OnRoadSegment(firstSegment, vehicle.CurrentLane, 0.99)
                    CurrentRouteIndex = Some 0
                    Status = VehicleMoving })

        let moved = prepared |> Transport.tick 1 |> firstVehicle

        match moved.CurrentPosition with
        | OnRoadSegment(segmentId, _, _) -> Assert.Equal(zeroDelayRoute.SegmentIds[1], segmentId)
        | other -> Assert.Fail($"Expected next segment, got {other}.")

    [<Fact>]
    let ``VehicleWaitsAtSignalizedIntersection`` () =
        let world = morningWorld ()
        let vehicle = firstVehicle world
        let route = world.Transport.Trips[vehicle.Trip].CurrentRoute.Value

        let prepared =
            world
            |> withFirstVehicle (fun vehicle ->
                { vehicle with
                    CurrentPosition = OnRoadSegment(route.SegmentIds[0], vehicle.CurrentLane, 0.99)
                    CurrentRouteIndex = Some 0
                    Status = VehicleMoving })

        let moved = prepared |> Transport.tick 1 |> firstVehicle

        match moved.CurrentPosition with
        | WaitingAtIntersection _ -> Assert.Equal(VehicleWaitingAtIntersection, moved.Status)
        | other -> Assert.Fail($"Expected signalized intersection wait, got {other}.")

    [<Fact>]
    let ``CompletedTripRemovesVehicleFromActiveTraffic`` () =
        let world = morningWorld ()
        let vehicle = firstVehicle world
        let route = world.Transport.Trips[vehicle.Trip].CurrentRoute.Value
        let lastIndex = route.SegmentIds.Length - 1

        let prepared =
            world
            |> withFirstVehicle (fun vehicle ->
                { vehicle with
                    CurrentPosition = OnRoadSegment(route.SegmentIds[lastIndex], vehicle.CurrentLane, 0.99)
                    CurrentRouteIndex = Some lastIndex
                    Status = VehicleMoving })

        let completed = prepared |> Transport.tick 1
        let completedVehicle = firstVehicle completed
        let frame = TrafficVisualization.getTrafficFrame completed

        Assert.True(completedVehicle.Status = VehicleCompleted || completedVehicle.Status = VehicleParked)
        Assert.DoesNotContain(frame.RoadSegments, fun segment -> segment.ActiveVehicleCount > 0 && segment.SegmentId = route.SegmentIds[lastIndex])

    [<Fact>]
    let ``VehiclePositionIsDeterministic`` () =
        let positions () =
            let world = TestWorld.create ()
            let origin =
                world.Map.Places
                |> Map.toSeq
                |> Seq.find (fun (_, place) -> place.Kind = Residence)
                |> fst

            let destination =
                world.Map.Places
                |> Map.toSeq
                |> Seq.find (fun (_, place) -> place.Kind = Workplace)
                |> fst

            world |> privateCarTripWorld origin destination |> Transport.tick 0 |> Transport.tick 1 |> firstVehicle |> fun vehicle -> sprintf "%A" vehicle.CurrentPosition

        Assert.Equal(positions (), positions ())

    [<Fact>]
    let ``TrafficFrameContainsActiveVehicles`` () =
        let world = morningWorld ()
        let frame = TrafficVisualization.getTrafficFrame world

        Assert.NotEmpty(frame.Vehicles)
        Assert.Contains(frame.Vehicles, fun vehicle -> vehicle.SegmentId.IsSome || vehicle.IntersectionId.IsSome)

    [<Fact>]
    let ``TrafficFrameCanQueryVehiclesBySegment`` () =
        let initial = TestWorld.create ()
        let origin =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Residence)
            |> fst

        let destination =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let world = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let vehicle = TrafficVisualization.getTrafficFrame world |> _.Vehicles |> List.find (fun vehicle -> vehicle.SegmentId.IsSome)
        let onSegment = TrafficVisualization.getVehiclesOnRoadSegment world vehicle.SegmentId.Value

        Assert.Contains(onSegment, fun candidate -> candidate.VehicleId = vehicle.VehicleId)

    [<Fact>]
    let ``TrafficFrameDiffReportsUpdatedVehicle`` () =
        let initial = TestWorld.create ()
        let origin =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Residence)
            |> fst

        let destination =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let before = TrafficVisualization.getTrafficFrame started
        let afterWorld = started |> Transport.tick 1
        let after = TrafficVisualization.getTrafficFrame afterWorld
        let diff = TrafficVisualization.diffTrafficFrames before after

        Assert.NotEmpty(diff.UpdatedVehicles)

    [<Fact>]
    let ``VehicleRenderPositionComesFromRoadGeometry`` () =
        let initial = TestWorld.create ()
        let origin =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Residence)
            |> fst

        let destination =
            initial.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let world = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let vehicle = firstVehicle world
        let frameVehicle = TrafficVisualization.getVehicleView world vehicle.Id |> Option.get
        let route = world.Transport.Trips[vehicle.Trip].CurrentRoute.Value
        let segment = world.Map.RoadSegments |> List.find (fun segment -> segment.Id = route.SegmentIds[0])
        let expectedStart = world.Map.RoadNodes[route.NodePath[0]].Position

        Assert.Equal(segment.Id, frameVehicle.SegmentId.Value)
        Assert.Equal(expectedStart.X, frameVehicle.Position.RenderX, 6)
        Assert.Equal(expectedStart.Y, frameVehicle.Position.RenderY, 6)

    [<Fact>]
    let ``DriverDoesNotChooseExitLaneTwentyMilesEarly`` () =
        let world = morningWorld ()
        let vehicles = world.Transport.Vehicles |> Map.toSeq |> Seq.map snd |> Seq.toList

        Assert.NotEmpty(vehicles)
        Assert.All(vehicles, fun vehicle ->
            Assert.True(vehicle.DistanceToManeuverMeters <= 2500.0, "Lane preparation should stay tactical in the vertical slice."))

    [<Fact>]
    let ``FailedMergeCanCreateMemory`` () =
        let world = morningWorld ()
        let vehicle =
            world.Transport.Vehicles
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.find (fun vehicle -> vehicle.CurrentLane.IsSome)

        let fromLane = vehicle.CurrentLane.Value
        let toLane = world.Transport.Lanes |> Map.toSeq |> Seq.map fst |> Seq.find ((<>) fromLane)
        let event = TransportEventOccurred(TestIds.eventId 10, LaneChangeFailed(vehicle.Id, fromLane, toLane))
        let next = SimulationPipeline.applyEventAndRemember event world

        Assert.Contains(next.Memories |> Map.toSeq |> Seq.map snd, fun memory -> Set.contains "merge" memory.Tags)
        Invariants.checkWorld next |> ignore

    [<Fact>]
    let ``LaneAndIntersectionRulesAreValid`` () =
        let world = TestWorld.create ()

        world.Transport.Lanes
        |> Map.iter (fun _ lane ->
            Assert.NotEmpty(lane.AllowedModes)
            Assert.NotEmpty(lane.PermittedMovements)
            Assert.True(lane.CapacityPerHour > 0.0))

        world.Transport.Intersections
        |> Map.iter (fun _ intersection ->
            Assert.True(intersection.CapacityPerMinute > 0)
            Assert.NotEmpty(intersection.PermittedMovements)
            Assert.InRange(intersection.QueueSpillbackRisk, 0.0, 1.0))

        Invariants.checkWorld world |> ignore
