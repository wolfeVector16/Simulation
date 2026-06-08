namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain
open RealSim.Avalonia.Services

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
                    Movements = Map.empty
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

    let private routedPlacePair world =
        let places =
            world.Map.Places
            |> Map.toSeq
            |> Seq.choose (fun (placeId, _) ->
                MapGraph.resolveRoadAccess world.Map placeId
                |> Option.map (fun access -> placeId, access.Node))
            |> Seq.toList

        seq {
            for origin, originNode in places do
                for destination, destinationNode in places do
                    if origin <> destination && originNode <> destinationNode then
                        origin, destination
        }
        |> Seq.filter (fun (origin, destination) -> TransportRouting.roadRoute world PrivateCar origin destination |> Option.isSome)
        |> Seq.sortByDescending (fun (origin, destination) -> MapGraph.distanceMeters world.Map world.Map.Places[origin].Position world.Map.Places[destination].Position)
        |> Seq.head

    let private walkingPlacePair world =
        let places =
            world.Map.Places
            |> Map.toSeq
            |> Seq.map fst
            |> Seq.toList

        seq {
            for origin in places do
                for destination in places do
                    if origin <> destination then
                        origin, destination
        }
        |> Seq.filter (fun (origin, destination) ->
            MapGraph.distanceMeters world.Map world.Map.Places[origin].Position world.Map.Places[destination].Position <= 3200.0
            && TransportRouting.roadRoute world Walk origin destination |> Option.isSome)
        |> Seq.sortBy (fun (origin, destination) -> MapGraph.distanceMeters world.Map world.Map.Places[origin].Position world.Map.Places[destination].Position)
        |> Seq.head

    let private walkingTripWorld origin destination world =
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
                          Purpose = ToLeisure
                          RemainingMinutes = 45
                          TotalMinutes = 45 } }

        { world with
            Sims = Map.add simId sim world.Sims
            Households = Map.add sim.Household { household with TransportationAccess = 0.0; Assets = 0m } world.Households
            Transport =
                { world.Transport with
                    TransitRoutes = Map.empty
                    Trips = Map.empty
                    Movements = Map.empty
                    Vehicles = Map.empty
                    RecentEvents = [] } }

    let private firstMovement world =
        world.Transport.Movements |> Map.toSeq |> Seq.map snd |> Seq.head

    [<Fact>]
    let ``CarTripCreatesMovementState`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let result = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement result
        let route = firstCurrentRoute result

        Assert.Equal(route.Id, movement.RouteId)
        Assert.Equal(MovementStatus.InProgress, movement.Status)
        Assert.True(movement.TotalDistanceMeters > 0.0)
        Assert.True(movement.Route.Geometry.Polyline.Length >= 2)
        Assert.Contains(result.Transport.Movements |> Map.toSeq |> Seq.map snd, fun candidate ->
            match candidate.Kind with
            | MovingEntityKind.Vehicle _ -> true
            | _ -> false)

    [<Fact>]
    let ``WalkingTripCreatesPedestrianMovementWhenRouteExists`` () =
        let world = TestWorld.create ()
        let origin, destination = walkingPlacePair world
        let result = world |> walkingTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement result

        match movement.Kind with
        | MovingEntityKind.Pedestrian _ -> Assert.Equal(Walk, movement.Route.Mode)
        | other -> Assert.Fail($"Expected pedestrian movement, got {other}.")

    [<Fact>]
    let ``FailedRouteDoesNotCreateCompletedMovement`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let disconnected = { world with Map = { world.Map with RoadSegments = [] } }
        let result = disconnected |> privateCarTripWorld origin destination |> Transport.tick 0

        Assert.Empty(result.Transport.Movements)
        Assert.DoesNotContain(result.Transport.Vehicles |> Map.toSeq |> Seq.map snd, fun vehicle -> vehicle.Status = VehicleCompleted)

    [<Fact>]
    let ``MovementStateHasRenderablePosition`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let movement = world |> privateCarTripWorld origin destination |> Transport.tick 0 |> firstMovement

        Assert.False(Double.IsNaN(movement.CurrentPosition.X))
        Assert.False(Double.IsNaN(movement.CurrentPosition.Y))
        Assert.InRange(movement.Progress, 0.0, 1.0)
        Assert.True(movement.HeadingRadians.IsSome)

    [<Fact>]
    let ``SameSeedMovementCreationIsDeterministic`` () =
        let movementText () =
            let world = TestWorld.create ()
            let origin, destination = routedPlacePair world
            let result = world |> privateCarTripWorld origin destination |> Transport.tick 0

            result.Transport.Movements
            |> Map.toSeq
            |> Seq.map (fun (movementId, movement) -> sprintf "%A|%A|%.3f|%.3f|%.3f" movementId movement.Kind movement.CurrentPosition.X movement.CurrentPosition.Y movement.Progress)
            |> Seq.toList

        Assert.Equal<string list>(movementText (), movementText ())

    [<Fact>]
    let ``MovementAdvancesEveryTick`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let started = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let before = firstMovement started
        let after = started |> Transport.tick 1 |> firstMovement

        Assert.True(after.Progress > before.Progress)
        Assert.True(after.DistanceOnLegMeters >= before.DistanceOnLegMeters || after.CurrentLegIndex > before.CurrentLegIndex)

    [<Fact>]
    let ``MovementCurrentPositionChangesOnTick`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let started = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let before = firstMovement started
        let after = started |> Transport.tick 1 |> firstMovement

        Assert.NotEqual(before.CurrentPosition, after.CurrentPosition)
        Assert.Equal(Some before.CurrentPosition, after.PreviousPosition)

    [<Fact>]
    let ``MovementCompletesAtDestination`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let started = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let completed = started |> Transport.tick 10_000
        let movement = firstMovement completed
        let trip = completed.Transport.Trips[movement.TripId]
        let destinationPosition = completed.Map.Places[destination].Position

        Assert.Equal(MovementStatus.Completed, movement.Status)
        Assert.Equal(TripStatus.Completed, trip.Status)
        Assert.True(MapGraph.distanceMeters completed.Map movement.CurrentPosition destinationPosition < 1.0)
        Assert.Contains(completed.Transport.RecentEvents, function MovementCompleted _ -> true | _ -> false)
        Assert.Contains(completed.Transport.RecentEvents, function TripCompleted tripId when tripId = trip.Id -> true | _ -> false)

    [<Fact>]
    let ``MovementDoesNotTeleportWhenBlocked`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let started = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let brokenRoute = { movement.Route with Geometry = { movement.Route.Geometry with Polyline = [] } }
        let blockedMovement = { movement with Route = brokenRoute; PreviousPosition = None; Status = MovementStatus.InProgress }
        let prepared = { started with Transport = { started.Transport with Movements = Map.add movement.Id blockedMovement started.Transport.Movements } }
        let after = MovementSystem.tick 1 prepared
        let next = after.Transport.Movements[movement.Id]

        Assert.Equal(MovementStatus.Failed, next.Status)
        Assert.Equal(movement.CurrentPosition, next.CurrentPosition)
        Assert.Equal(Some movement.CurrentPosition, next.PreviousPosition)
        Assert.Contains(after.Transport.RecentEvents, function MovementFailed(id, _) when id = movement.Id -> true | _ -> false)

    [<Fact>]
    let ``MovementUsesRouteGeometry`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let started = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let after = started |> Transport.tick 1 |> firstMovement
        let expected = TransportRoute.interpolate after.Progress after.Route |> Option.get

        Assert.Equal(expected.X, after.CurrentPosition.X, 6)
        Assert.Equal(expected.Y, after.CurrentPosition.Y, 6)

    [<Fact>]
    let ``SameMovementTickSequenceIsDeterministic`` () =
        let sequence () =
            let world = TestWorld.create ()
            let origin, destination = routedPlacePair world
            let final = world |> privateCarTripWorld origin destination |> Transport.tick 0 |> Transport.tick 1 |> Transport.tick 1 |> Transport.tick 1

            final.Transport.Movements
            |> Map.toSeq
            |> Seq.map (fun (id, movement) -> sprintf "%A|%.4f|%.4f|%.4f|%A" id movement.CurrentPosition.X movement.CurrentPosition.Y movement.Progress movement.Status)
            |> Seq.toList

        Assert.Equal<string list>(sequence (), sequence ())

    [<Fact>]
    let ``TransportUsesSharedRoutingPipeline`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let sharedRoute = TransportRouting.roadRoute world PrivateCar origin destination |> Option.get
        let result = world |> privateCarTripWorld origin destination |> Transport.tick 0
        let transportRoute = firstCurrentRoute result

        Assert.Equal<RoadSegmentId list>(sharedRoute.Segments |> List.map _.Id, TransportRoute.segmentIds transportRoute)
        Assert.Equal<Coordinates list>(sharedRoute.Geometry.Polyline, transportRoute.Geometry.Polyline)
        Invariants.checkWorld result |> ignore

    [<Fact>]
    let ``TransportRouteHasNonEmptyGeometry`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let route = world |> privateCarTripWorld origin destination |> Transport.tick 0 |> firstCurrentRoute

        Assert.NotEmpty(route.Legs)
        Assert.True(route.Geometry.Polyline.Length >= 2)
        Assert.True(route.TotalDistanceMeters > 0.0)

    [<Fact>]
    let ``RouteGeometryStartsNearOriginAndEndsNearDestination`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let route = world |> privateCarTripWorld origin destination |> Transport.tick 0 |> firstCurrentRoute
        let first = route.Geometry.Polyline.Head
        let last = route.Geometry.Polyline |> List.last
        let originPlace = world.Map.Places[origin]
        let destinationPlace = world.Map.Places[destination]

        Assert.True(MapGraph.distanceMeters world.Map first originPlace.Position < 1.0)
        Assert.True(MapGraph.distanceMeters world.Map last destinationPlace.Position < 1.0)

    [<Fact>]
    let ``WalkingRouteRequiresPedestrianNetworkOrFails`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world

        match TransportRouting.route world Walk origin destination with
        | RouteFailed PedestrianNetworkUnavailable -> Assert.True(true)
        | other -> Assert.Fail($"Expected pedestrian network failure, got {other}.")

    [<Fact>]
    let ``BikeRouteRequiresBikeNetworkOrFails`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world

        match TransportRouting.route world Bike origin destination with
        | RouteFailed BikeNetworkUnavailable -> Assert.True(true)
        | other -> Assert.Fail($"Expected bike network failure, got {other}.")

    [<Fact>]
    let ``RouteInterpolationReturnsStartMiddleEnd`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let route = world |> privateCarTripWorld origin destination |> Transport.tick 0 |> firstCurrentRoute
        let start = TransportRoute.interpolate 0.0 route |> Option.get
        let middle = TransportRoute.interpolate 0.5 route |> Option.get
        let finish = TransportRoute.interpolate 1.0 route |> Option.get

        Assert.Equal(route.Geometry.Polyline.Head.X, start.X, 6)
        Assert.Equal(route.Geometry.Polyline.Head.Y, start.Y, 6)
        Assert.Equal((route.Geometry.Polyline |> List.last).X, finish.X, 6)
        Assert.Equal((route.Geometry.Polyline |> List.last).Y, finish.Y, 6)
        Assert.True(MapGraph.distanceMeters world.Map start middle > 0.0)
        Assert.True(MapGraph.distanceMeters world.Map middle finish > 0.0)

    [<Fact>]
    let ``DescribeRouteStillWorks`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let route = MapGraph.findRoute world.Map origin destination |> Option.get
        let description = MapGraph.describeRoute world.Map route

        Assert.Contains("road graph", description)
        Assert.Contains("km", description)

    [<Fact>]
    let ``RoadAccessResolutionIsShared`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let originAccess = MapGraph.resolveRoadAccess world.Map origin |> Option.get
        let destinationAccess = MapGraph.resolveRoadAccess world.Map destination |> Option.get
        let route = TransportRouting.roadRoute world PrivateCar origin destination |> Option.get

        Assert.Equal(originAccess.Node, route.NodePath.Head)
        Assert.Equal(destinationAccess.Node, route.NodePath |> List.last)
        Assert.True(route.AccessMeters >= originAccess.AccessMeters + destinationAccess.AccessMeters)

    [<Fact>]
    let ``RouteFailureDoesNotFallbackToStraightLine`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let disconnected = { world with Map = { world.Map with RoadSegments = [] } }

        Assert.True((TransportRouting.roadRoute disconnected PrivateCar origin destination).IsNone)
        Assert.True((MapGraph.findRoute disconnected.Map origin destination).IsNone)

    [<Fact>]
    let ``RoadRouteUsesMapGraphPath`` () =
        let world = TestWorld.create ()
        let origin, destination = routedPlacePair world
        let originAccess = MapGraph.resolveRoadAccess world.Map origin |> Option.get
        let destinationAccess = MapGraph.resolveRoadAccess world.Map destination |> Option.get
        let edgeCost node previousSegment (edge: MapGraph.RoadEdge) =
            TransportRouting.segmentTravelMinutes world edge.Segment
            + float (TransportRouting.intersectionDelayMinutes world PrivateCar node previousSegment edge.Segment)

        let mapGraphPath =
            MapGraph.shortestRoadPathWithCost world.Map originAccess.Node destinationAccess.Node edgeCost
            |> Option.get

        let route = TransportRouting.roadRoute world PrivateCar origin destination |> Option.get

        Assert.Equal<RoadSegmentId list>(mapGraphPath.Steps |> List.map _.Segment.Id, route.Segments |> List.map _.Id)
        Assert.Equal<RoadNodeId list>(mapGraphPath.NodePath, route.NodePath)

    [<Fact>]
    let ``ExistingScenarioStillRunsOneTick`` () =
        let world = TestWorld.create () |> TestWorld.tick 1

        Invariants.checkWorld world |> ignore

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
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 800.0)
            |> fst

        let origin =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        let world, express = addParallelRoad 10 "Audit Express Link" Highway 120.0 160 highway.From highway.To world
        let result = world |> privateCarTripWorld origin destination |> Transport.tick 15
        let route = firstCurrentRoute result

        Assert.Contains(express.Id, TransportRoute.segmentIds route)
        Invariants.checkWorld result |> ignore

    [<Fact>]
    let ``CongestionCanMakeHighwaySlowerThanLocalRoute`` () =
        let world = TestWorld.create ()
        let highway = world.Map.RoadSegments |> List.find (fun segment -> segment.RoadClass = Highway)
        let destination =
            world.Map.Places
            |> Map.toSeq
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 800.0)
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

        Assert.Contains(localDetour.Id, TransportRoute.segmentIds route)
        Assert.DoesNotContain(highway.Id, TransportRoute.segmentIds route)
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

        let baseWorld, fastMiddle = baseWorld |> addNode 400 800.0 500.0
        let baseWorld, slowerMiddle = baseWorld |> addNode 401 800.0 420.0
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
            |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 800.0)
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

        Assert.Contains(fastA.Id, TransportRoute.segmentIds withoutDelay)
        Assert.Contains(fastB.Id, TransportRoute.segmentIds withoutDelay)
        Assert.Contains(slowA.Id, TransportRoute.segmentIds withDelay)
        Assert.Contains(slowB.Id, TransportRoute.segmentIds withDelay)

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
                |> Seq.find (fun (_, place) -> place.Kind = Commercial && place.Position.X > 800.0)
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
            TransportRoute.segmentIds route
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
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let world = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let vehicle = firstVehicle world

        match vehicle.CurrentPosition with
        | OnRoadSegment(segmentId, laneId, progress) ->
            Assert.Contains(segmentId, vehicle.Trip |> fun tripId -> TransportRoute.segmentIds world.Transport.Trips[tripId].CurrentRoute.Value)
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
        let zeroDelayRoute =
            { route with
                Legs =
                    route.Legs
                    |> List.map (fun leg ->
                        if leg.SegmentId.IsSome then
                            { leg with IntersectionDelayMinutes = 0; ExpectedMinutes = leg.SegmentTravelMinutes }
                        else
                            leg) }
        let trip = world.Transport.Trips[vehicle.Trip]

        let prepared =
            { world with
                Transport =
                    { world.Transport with
                        Trips = Map.add vehicle.Trip { trip with CurrentRoute = Some zeroDelayRoute; PlannedRoute = Some zeroDelayRoute } world.Transport.Trips } }
            |> withFirstVehicle (fun vehicle ->
                let segmentIds = TransportRoute.segmentIds zeroDelayRoute
                let firstSegment = segmentIds[0]
                { vehicle with
                    CurrentPosition = OnRoadSegment(firstSegment, vehicle.CurrentLane, 0.99)
                    CurrentRouteIndex = Some 0
                    Status = VehicleMoving })

        let moved = prepared |> Transport.tick 1 |> firstVehicle

        match moved.CurrentPosition with
        | OnRoadSegment(segmentId, _, _) -> Assert.Equal((TransportRoute.segmentIds zeroDelayRoute)[1], segmentId)
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
                    CurrentPosition = OnRoadSegment((TransportRoute.segmentIds route)[0], vehicle.CurrentLane, 0.99)
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
        let segmentIds = TransportRoute.segmentIds route
        let lastIndex = segmentIds.Length - 1

        let prepared =
            world
            |> withFirstVehicle (fun vehicle ->
                { vehicle with
                    CurrentPosition = OnRoadSegment(segmentIds[lastIndex], vehicle.CurrentLane, 0.99)
                    CurrentRouteIndex = Some lastIndex
                    Status = VehicleMoving })

        let completed = prepared |> Transport.tick 1
        let completedVehicle = firstVehicle completed
        let frame = TrafficVisualization.getTrafficFrame completed

        Assert.True(completedVehicle.Status = VehicleCompleted || completedVehicle.Status = VehicleParked)
        Assert.DoesNotContain(frame.RoadSegmentTrafficViews, fun segment -> segment.ActiveVehicleCount > 0 && segment.SegmentId = segmentIds[lastIndex])

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
    let ``TrafficFrameContainsActiveVehiclesAndPedestrians`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let carWorld = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let vehicleMovement = firstMovement carWorld
        let simId = carWorld.Sims |> Map.toSeq |> Seq.head |> fst
        let pedestrianMovement =
            { vehicleMovement with
                Id = MovementId(guid 9101)
                Kind = MovingEntityKind.Pedestrian simId
                CurrentSpeedKph = 4.5 }
        let world =
            { carWorld with
                Transport =
                    { carWorld.Transport with
                        Movements = Map.add pedestrianMovement.Id pedestrianMovement carWorld.Transport.Movements } }
        let frame = TrafficVisualization.getTrafficFrame world

        Assert.NotEmpty(frame.MovingEntities)
        Assert.NotEmpty(frame.Vehicles)
        Assert.NotEmpty(frame.Pedestrians)
        Assert.All(frame.MovingEntities, fun entity -> Assert.NotEmpty(entity.RoutePreview))

    [<Fact>]
    let ``SegmentCongestionReflectsActiveVehicleCount`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let segmentIndex, segmentId =
            movement.Route.Legs
            |> List.indexed
            |> List.pick (fun (index, leg) -> leg.SegmentId |> Option.map (fun segmentId -> index, segmentId))
        let copy =
            { movement with
                Id = MovementId(guid 9201)
                CurrentLegIndex = segmentIndex
                CurrentSpeedKph = 18.0 }
        let original =
            { movement with
                CurrentLegIndex = segmentIndex
                CurrentSpeedKph = 30.0 }
        let prepared =
            { started with
                Transport =
                    { started.Transport with
                        Movements =
                            started.Transport.Movements
                            |> Map.add original.Id original
                            |> Map.add copy.Id copy } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }
        let segment = updated.Map.RoadSegments |> List.find (fun segment -> segment.Id = segmentId)
        let expected = 2.0 / max 1.0 (float segment.CapacityPerMinute * 15.0)

        Assert.Equal(expected, updated.Transport.SegmentCongestion[segmentId], 6)
        Assert.Equal(2, updated.Transport.Metrics.ActiveVehicleCount)

    [<Fact>]
    let ``IntersectionWaitingCountReflectsMovements`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let segmentIndex, nodeId =
            movement.Route.Legs
            |> List.indexed
            |> List.pick (fun (index, leg) -> leg.SegmentId |> Option.bind (fun _ -> leg.ToRoadNode |> Option.map (fun nodeId -> index, nodeId)))
        let preparedWorld = started |> addIntersection nodeId StopSign 0.0 0.0
        let waiting =
            { movement with
                CurrentLegIndex = segmentIndex
                Status = MovementStatus.WaitingAtIntersection
                CurrentSpeedKph = 0.0
                DelaySeconds = 75 }
        let prepared =
            { preparedWorld with
                Transport =
                    { preparedWorld.Transport with
                        Movements = Map.add waiting.Id waiting preparedWorld.Transport.Movements } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }
        let frame = TrafficVisualization.getTrafficFrame updated
        let view = frame.IntersectionTrafficViews |> List.find (fun view -> view.IntersectionId = nodeId)

        Assert.Equal(1, updated.Transport.Metrics.IntersectionWaitingCount)
        Assert.Equal(1, view.WaitingVehicleCount)

    [<Fact>]
    let ``TrafficSystemDoesNotRoute`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let beforeDiagnostics = started.PerformanceDiagnostics
        let beforeTrips = started.Transport.Trips
        let beforeEvents = started.Transport.RecentEvents
        let updated = { started with Transport = TrafficSystem.updateTransportState started started.Transport }

        Assert.Equal(beforeDiagnostics, updated.PerformanceDiagnostics)
        Assert.True((beforeTrips = updated.Transport.Trips))
        let addedEvents = updated.Transport.RecentEvents |> List.skip beforeEvents.Length
        Assert.DoesNotContain(addedEvents, function RouteChosen _ | RouteReplanned _ -> true | _ -> false)

    [<Fact>]
    let ``TrafficFrameRoadViewsMatchMovementOccupancy`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let segmentIndex, segmentId =
            movement.Route.Legs
            |> List.indexed
            |> List.pick (fun (index, leg) -> leg.SegmentId |> Option.map (fun segmentId -> index, segmentId))
        let movementA = { movement with CurrentLegIndex = segmentIndex; CurrentSpeedKph = 20.0 }
        let movementB = { movementA with Id = MovementId(guid 9301); CurrentSpeedKph = 40.0 }
        let prepared =
            { started with
                Transport =
                    { started.Transport with
                        Movements =
                            started.Transport.Movements
                            |> Map.add movementA.Id movementA
                            |> Map.add movementB.Id movementB } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }
        let frame = TrafficVisualization.getTrafficFrame updated
        let roadView = frame.RoadSegmentTrafficViews |> List.find (fun view -> view.SegmentId = segmentId)

        Assert.Equal(2, roadView.ActiveVehicleCount)
        Assert.Equal(30.0, roadView.AverageSpeedKph, 6)
        Assert.Equal(updated.Transport.SegmentCongestion[segmentId], roadView.Congestion)

    [<Fact>]
    let ``MovementSpeedAffectedByCongestionIfEnabled`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let segmentIndex, segmentId =
            movement.Route.Legs
            |> List.indexed
            |> List.pick (fun (index, leg) -> leg.SegmentId |> Option.map (fun segmentId -> index, segmentId))
        let preparedMovement = { movement with CurrentLegIndex = segmentIndex; DistanceOnLegMeters = 0.0; Progress = 0.0 }
        let prepared =
            { started with
                Transport =
                    { started.Transport with
                        Movements = Map.add preparedMovement.Id preparedMovement started.Transport.Movements } }
        let uncongested = prepared |> MovementSystem.tick 1 |> firstMovement
        let congestedWorld =
            { prepared with
                Transport =
                    { prepared.Transport with
                        SegmentCongestion = Map.add segmentId 1.0 prepared.Transport.SegmentCongestion } }
        let congested = congestedWorld |> MovementSystem.tick 1 |> firstMovement

        Assert.True(congested.CurrentSpeedKph < uncongested.CurrentSpeedKph)
        Assert.True(congested.Progress < uncongested.Progress)

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
        let movement = firstMovement world
        let segmentIndex, segmentId =
            movement.Route.Legs
            |> List.indexed
            |> List.pick (fun (index, leg) -> leg.SegmentId |> Option.map (fun segmentId -> index, segmentId))
        let preparedMovement = { movement with CurrentLegIndex = segmentIndex }
        let prepared =
            { world with
                Transport =
                    { world.Transport with
                        Movements = Map.add movement.Id preparedMovement world.Transport.Movements } }
        let onSegment = TrafficVisualization.getVehiclesOnRoadSegment prepared segmentId

        Assert.Contains(onSegment, fun candidate -> candidate.MovementId = movement.Id && candidate.VehicleId.IsSome)

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
    let ``TrafficFrameUsesMovementStatePositions`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let world = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement world
        let frame = TrafficVisualization.getTrafficFrame world
        let entity = frame.MovingEntities |> List.find (fun entity -> entity.MovementId = movement.Id)

        Assert.Equal(movement.CurrentPosition.X, entity.CurrentPosition.X, 6)
        Assert.Equal(movement.CurrentPosition.Y, entity.CurrentPosition.Y, 6)
        Assert.Equal(movement.PreviousPosition, entity.PreviousPosition)

    [<Fact>]
    let ``TrafficVisualizationDoesNotReconstructRoutesIndependently`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let routePreview = [ { X = 11.0; Y = 22.0 }; { X = 33.0; Y = 44.0 }; { X = 55.0; Y = 66.0 } ]
        let route = { movement.Route with Legs = []; Geometry = { Polyline = routePreview; DistanceMeters = 70.0 } }
        let edited = { movement with Route = route; CurrentPosition = routePreview[1]; PreviousPosition = Some routePreview[0] }
        let world = { started with Transport = { started.Transport with Movements = Map.add movement.Id edited started.Transport.Movements } }
        let frame = TrafficVisualization.getTrafficFrame world
        let entity = frame.MovingEntities |> List.find (fun entity -> entity.MovementId = movement.Id)

        Assert.Equal(sprintf "%A" routePreview, sprintf "%A" entity.RoutePreview)
        Assert.Equal(routePreview[1], entity.CurrentPosition)
        Assert.DoesNotContain(frame.RoadSegmentTrafficViews, fun segment -> segment.ActiveVehicleCount > 0)

    [<Fact>]
    let ``AvaloniaProjectionUsesTrafficFrameMovement`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let started = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement started
        let routePreview = movement.Route.Geometry.Polyline
        let destinationPoint = routePreview |> List.last
        let edited = { movement with CurrentPosition = destinationPoint; Progress = 0.0 }
        let world = { started with Transport = { started.Transport with Movements = Map.add movement.Id edited started.Transport.Movements } }
        let projection = MapProjection.Project world
        let entity = projection.MovingEntities |> Seq.find (fun entity -> entity.Id = movement.Id.ToString())

        Assert.Equal(entity.Destination.X, entity.CurrentPosition.X, 6)
        Assert.Equal(entity.Destination.Y, entity.CurrentPosition.Y, 6)

    [<Fact>]
    let ``SelectedVehicleCanExposeCurrentPosition`` () =
        let initial = TestWorld.create ()
        let origin, destination = routedPlacePair initial
        let world = initial |> privateCarTripWorld origin destination |> Transport.tick 0
        let movement = firstMovement world
        let vehicleId =
            match movement.Kind with
            | MovingEntityKind.Vehicle vehicleId -> vehicleId
            | MovingEntityKind.EmergencyResponder vehicleId
            | MovingEntityKind.FreightVehicle vehicleId
            | MovingEntityKind.ServiceVehicle vehicleId -> vehicleId
            | other -> failwithf "Expected vehicle movement, got %A" other
        let view = TrafficVisualization.getVehicleView world vehicleId |> Option.get

        Assert.Equal(movement.CurrentPosition.X, view.Position.RenderX, 6)
        Assert.Equal(movement.CurrentPosition.Y, view.Position.RenderY, 6)

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
        let movement = firstMovement world

        Assert.Equal(movement.CurrentPosition.X, frameVehicle.Position.RenderX, 6)
        Assert.Equal(movement.CurrentPosition.Y, frameVehicle.Position.RenderY, 6)

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
