namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain

module MesoscopicTrafficTests =
    let private guid suffix =
        Guid.Parse(sprintf "31000000-0000-0000-0000-%012x" suffix)

    let private routedPlacePair world =
        let places =
            world.Map.Places
            |> Map.toSeq
            |> Seq.toList

        let origin =
            places
            |> List.find (fun (_, place) -> place.Kind = Residence)
            |> fst

        let destination =
            places
            |> List.find (fun (_, place) -> place.Kind = Workplace)
            |> fst

        origin, destination

    let private startCarTrip world =
        let origin, destination = routedPlacePair world
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
                    Trips = Map.empty
                    Movements = Map.empty
                    Vehicles = Map.empty
                    RecentEvents = [] } }
        |> Transport.tick 0

    let private firstMovement world : MovementState =
        world.Transport.Movements |> Map.toSeq |> Seq.map snd |> Seq.head

    let private firstVehicle world : VehicleState =
        world.Transport.Vehicles |> Map.toSeq |> Seq.map snd |> Seq.head

    let private segmentWithTwoLanes world : RoadSegment =
        world.Map.RoadSegments
        |> List.find (fun segment -> segment.LaneIds.Length >= 2)

    let private movementOnSegment (_world: World) (movement: MovementState) (segment: RoadSegment) (laneId: LaneId) vehicleId : MovementState =
        let legIndex =
            movement.Route.Legs
            |> List.tryFindIndex (fun leg -> leg.SegmentId = Some segment.Id)
            |> Option.defaultValue movement.CurrentLegIndex

        let route =
            { movement.Route with
                Mode = movement.Route.Mode
                Legs =
                    movement.Route.Legs
                    |> List.mapi (fun index leg ->
                        if index = legIndex then { leg with LaneIds = [ laneId ] } else leg) }

        { movement with
            Id = MovementId(guid vehicleId)
            Kind = MovingEntityKind.Vehicle(VehicleId(guid (vehicleId + 1000)))
            Route = route
            CurrentLegIndex = legIndex
            DistanceOnLegMeters = max 0.0 (segment.LengthMeters * 0.40)
            Status = MovementStatus.InProgress
            CurrentSpeedKph = 24.0 }

    let private vehicleFor (movement: MovementState) (baseVehicle: VehicleState) laneId mode suffix : VehicleState =
        let vehicleId =
            match movement.Kind with
            | MovingEntityKind.Vehicle vehicleId -> vehicleId
            | _ -> VehicleId(guid suffix)
        let segmentId =
            movement.Route.Legs[movement.CurrentLegIndex].SegmentId
            |> Option.defaultWith (fun () -> RoadSegmentId(guid (suffix + 5000)))

        { baseVehicle with
            Id = vehicleId
            Mode = mode
            CurrentLane = Some laneId
            CurrentPosition = OnRoadSegment(segmentId, Some laneId, 0.40)
            Status = VehicleStatus.VehicleMoving
            NextRequiredMovement = Some MoveThrough
            DistanceToManeuverMeters = 120.0 }

    let private setLane laneId update world =
        { world with
            Transport =
                { world.Transport with
                    Lanes =
                        world.Transport.Lanes
                        |> Map.change laneId (Option.map update) } }

    [<Fact>]
    let ``TrafficFrameIncludesLaneOccupancy`` () =
        let world = TestWorld.create () |> startCarTrip
        let updated = { world with Transport = TrafficSystem.updateTransportState world world.Transport }
        let frame = TrafficVisualization.getTrafficFrame updated

        Assert.NotEmpty(frame.LaneOccupancies)
        Assert.Equal(updated.Transport.LaneOccupancies.Count, frame.LaneOccupancies.Length)

    [<Fact>]
    let ``VehiclesUseMultipleAvailableLanes`` () =
        let world = TestWorld.create () |> startCarTrip
        let segment = segmentWithTwoLanes world
        let lanes = segment.LaneIds
        let baseMovement = firstMovement world
        let baseVehicle = firstVehicle world
        let movementA = movementOnSegment world baseMovement segment lanes[0] 1
        let movementB = movementOnSegment world baseMovement segment lanes[1] 2
        let vehicleA = vehicleFor movementA baseVehicle lanes[0] PrivateCar 11
        let vehicleB = vehicleFor movementB baseVehicle lanes[1] PrivateCar 12
        let prepared =
            { world with
                Transport =
                    { world.Transport with
                        Movements = [ movementA.Id, movementA; movementB.Id, movementB ] |> Map.ofList
                        Vehicles = [ vehicleA.Id, vehicleA; vehicleB.Id, vehicleB ] |> Map.ofList } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }

        Assert.Contains(movementA.Id, updated.Transport.LaneOccupancies[lanes[0]].MovementIds)
        Assert.Contains(movementB.Id, updated.Transport.LaneOccupancies[lanes[1]].MovementIds)

    [<Fact>]
    let ``CarsDoNotUseBusOnlyLane`` () =
        let world = TestWorld.create () |> startCarTrip
        let segment = segmentWithTwoLanes world
        let generalLane, busLane = segment.LaneIds[0], segment.LaneIds[1]
        let prepared =
            world
            |> setLane generalLane (fun lane -> { lane with CurrentDensity = 0.95 })
            |> setLane busLane (fun lane -> { lane with LaneType = BusOnly; AllowedModes = [ Bus ] |> Set.ofList; PermittedMovements = [ MoveThrough; MoveRight ] |> Set.ofList; CurrentDensity = 0.0 })
            |> setLane generalLane (fun lane -> { lane with LaneType = General; CurrentDensity = 0.95 })
        let vehicle = firstVehicle prepared
        let movement = firstMovement prepared
        let prepared =
            { prepared with
                Transport =
                    { prepared.Transport with
                        Vehicles = Map.add vehicle.Id { vehicle with CurrentLane = Some generalLane; Mode = PrivateCar; NextRequiredMovement = Some MoveThrough } prepared.Transport.Vehicles
                        Movements = Map.add movement.Id { movement with Route = { movement.Route with Mode = PrivateCar } } prepared.Transport.Movements } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }

        Assert.NotEqual(Some busLane, updated.Transport.Vehicles[vehicle.Id].CurrentLane)

    [<Fact>]
    let ``BusUsesBusLaneWhenAvailable`` () =
        let world = TestWorld.create () |> startCarTrip
        let segment = segmentWithTwoLanes world
        let generalLane, busLane = segment.LaneIds[0], segment.LaneIds[1]
        let prepared =
            world
            |> setLane busLane (fun lane -> { lane with LaneType = BusOnly; AllowedModes = [ Bus ] |> Set.ofList })
        let vehicle = firstVehicle prepared
        let movement = firstMovement prepared
        let route =
            { movement.Route with
                Mode = Bus
                Legs = movement.Route.Legs |> List.map (fun leg -> if leg.SegmentId = Some segment.Id then { leg with LaneIds = segment.LaneIds } else leg) }
        let prepared =
            { prepared with
                Transport =
                    { prepared.Transport with
                        Vehicles = Map.add vehicle.Id { vehicle with CurrentLane = Some generalLane; Mode = Bus; NextRequiredMovement = Some MoveThrough } prepared.Transport.Vehicles
                        Movements =
                            Map.add movement.Id
                                { movement with
                                    Route = route
                                    CurrentLegIndex = route.Legs |> List.findIndex (fun leg -> leg.SegmentId = Some segment.Id)
                                    DistanceOnLegMeters = max 0.0 ((route.Legs |> List.find (fun leg -> leg.SegmentId = Some segment.Id)).DistanceMeters - 50.0) }
                                prepared.Transport.Movements } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }

        Assert.Equal(Some busLane, updated.Transport.Vehicles[vehicle.Id].CurrentLane)

    [<Fact>]
    let ``SignalPhaseAdvancesOverTicks`` () =
        let world = TestWorld.create ()
        let nodeId, intersection =
            world.Transport.Intersections
            |> Map.toSeq
            |> Seq.find (fun (_, intersection) ->
                not intersection.SignalPhases.IsEmpty
                && match intersection.Control with Signalized _ | AdaptiveSignal | TransitPrioritySignal -> true | _ -> false)
        let prepared =
            let phases =
                [ { Kind = ThroughPhase; DurationSeconds = 90; Movements = [ MoveThrough; MoveRight ] |> Set.ofList }
                  { Kind = ProtectedLeftPhase; DurationSeconds = 90; Movements = [ MoveLeft ] |> Set.ofList } ]
            { world with
                Transport =
                    { world.Transport with
                        Intersections = Map.add nodeId { intersection with SignalPhases = phases } world.Transport.Intersections
                        SignalPhaseStates =
                            Map.add nodeId
                                { IntersectionId = nodeId
                                  CurrentPhaseIndex = 0
                                  SecondsRemaining = 1
                                  CycleSeconds = phases |> List.sumBy _.DurationSeconds }
                                world.Transport.SignalPhaseStates } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }

        Assert.NotEqual(0, updated.Transport.SignalPhaseStates[nodeId].CurrentPhaseIndex)
        Assert.Contains(updated.Transport.RecentEvents, function SignalPhaseChanged(id, _) when id = nodeId -> true | _ -> false)

    [<Fact>]
    let ``DownstreamFullBlocksUpstreamEntry`` () =
        let world = TestWorld.create () |> startCarTrip
        let segment = segmentWithTwoLanes world
        let laneId = segment.LaneIds[0]
        let movement = firstMovement world
        let vehicle = firstVehicle world
        let queued = { movement with Status = MovementStatus.Queued; CurrentSpeedKph = 0.0 }
        let prepared =
            world
            |> setLane laneId (fun lane -> { lane with CapacityPerHour = 1.0 })
        let prepared =
            { prepared with
                Transport =
                    { prepared.Transport with
                        Movements = Map.add queued.Id queued prepared.Transport.Movements
                        Vehicles = Map.add vehicle.Id { vehicle with CurrentLane = Some laneId; Status = VehicleStatus.VehicleQueued } prepared.Transport.Vehicles } }
        let updated = { prepared with Transport = TrafficSystem.updateTransportState prepared prepared.Transport }

        Assert.True(updated.Transport.LaneOccupancies[laneId].Spillback)
        Assert.Contains(updated.Transport.RecentEvents, function QueueSpillbackStarted id when id = laneId -> true | _ -> false)

    [<Fact>]
    let ``TrafficFrameMatchesMovementState`` () =
        let world = TestWorld.create () |> startCarTrip
        let updated = { world with Transport = TrafficSystem.updateTransportState world world.Transport }
        let movement = firstMovement updated
        let entity = TrafficVisualization.getTrafficFrame updated |> _.MovingEntities |> List.find (fun entity -> entity.MovementId = movement.Id)

        Assert.Equal(movement.CurrentPosition.X, entity.CurrentPosition.X, 6)
        Assert.Equal(movement.CurrentPosition.Y, entity.CurrentPosition.Y, 6)
        Assert.Equal(TrafficVisualization.getMovingEntityView updated movement.Id |> Option.map _.LaneId, Some entity.LaneId)

    [<Fact>]
    let ``SameSeedLaneChangesDeterministic`` () =
        let run () =
            let world = TestWorld.create () |> startCarTrip
            let updated = { world with Transport = TrafficSystem.updateTransportState world world.Transport }
            updated.Transport.LaneOccupancies, updated.Transport.RecentEvents |> List.map string

        Assert.Equal(run (), run ())
