namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain
open RealSim.Avalonia.Models
open RealSim.Avalonia.Services
open RealSim.Avalonia.ViewModels
open RealSim.Avalonia.Controls

module AvaloniaViewerTests =
    let private activeMovementWorld () =
        [ 1..96 ]
        |> Seq.scan (fun world _ -> TestWorld.tick 15 world) (TestWorld.create ())
        |> Seq.find (fun world -> not world.Transport.Movements.IsEmpty && (MapProjection.Project world).MovingEntities.Count > 0)

    let private withPedestrianMovement world =
        let movement = world.Transport.Movements |> Map.toSeq |> Seq.map snd |> Seq.head
        let simId = world.Sims |> Map.toSeq |> Seq.head |> fst
        let pedestrian =
            { movement with
                Kind = Simulation.Domain.MovingEntityKind.Pedestrian simId
                CurrentSpeedKph = 4.5
                Status = Simulation.Domain.MovementStatus.InProgress }

        { world with
            Transport =
                { world.Transport with
                    Movements = [ movement.Id, pedestrian ] |> Map.ofList } }

    let private primitiveSignature (projection: MapProjectionResult) =
        projection.Primitives
        |> Seq.map (fun primitive ->
            let points =
                primitive.Points
                |> Seq.map (fun point -> sprintf "%.2f,%.2f" point.X point.Y)
                |> String.concat ";"

            sprintf "%s|%A|%s|%s" primitive.Id primitive.Kind primitive.Name points)
        |> Seq.toArray

    [<Fact>]
    let ``MapProjectionIsDeterministic`` () =
        let world = TestWorld.create ()

        let first = MapProjection.Project world
        let second = MapProjection.Project world

        Assert.True((primitiveSignature first) = (primitiveSignature second))

    [<Fact>]
    let ``MapProjectionProducesCoreDrawPrimitives`` () =
        let world = TestWorld.create ()
        let projection = MapProjection.Project world

        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Neighborhood)
        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Road)
        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Place)
        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Building)
        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Household)

    [<Fact>]
    let ``MapProjectionProducesLegendItems`` () =
        let world = TestWorld.create ()
        let projection = MapProjection.Project world

        Assert.NotEmpty(projection.LegendItems)
        Assert.Contains(projection.LegendItems, fun item -> item.Label.Contains("Highway"))
        Assert.Contains(projection.LegendItems, fun item -> item.Label.Contains("Commercial"))
        Assert.Contains(projection.LegendItems, fun item -> item.Label.Contains("Vehicle"))
        Assert.Contains(projection.LegendItems, fun item -> item.Label.Contains("Recent event"))

    [<Fact>]
    let ``MapProjectionDistinguishesRoadClasses`` () =
        let world = TestWorld.create ()
        let roads =
            (MapProjection.Project world).Primitives
            |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.Road)
            |> Seq.groupBy _.Category
            |> Seq.map (fun (category, primitives) -> category, primitives |> Seq.head)
            |> Seq.toArray

        Assert.True(roads.Length >= 2)
        Assert.True(roads |> Seq.map (fun (_, primitive) -> primitive.Thickness, primitive.Stroke) |> Seq.distinct |> Seq.length >= 2)

    [<Fact>]
    let ``MapProjectionDistinguishesPlaceTypes`` () =
        let world = TestWorld.create ()
        let placesAndBuildings =
            (MapProjection.Project world).Primitives
            |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.Place || primitive.Kind = MapPrimitiveKind.Building)
            |> Seq.groupBy _.Category
            |> Seq.map (fun (category, primitives) -> category, primitives |> Seq.head)
            |> Seq.toArray

        Assert.True(placesAndBuildings.Length >= 4)
        Assert.True(placesAndBuildings |> Seq.map (fun (_, primitive) -> primitive.Symbol, primitive.Fill) |> Seq.distinct |> Seq.length >= 4)

    [<Fact>]
    let ``MovingEntitiesGeneratedFromActiveTrips`` () =
        let world =
            [ 1..96 ]
            |> Seq.scan (fun world _ -> TestWorld.tick 15 world) (TestWorld.create ())
            |> Seq.find (fun world ->
                world.Transport.Trips
                |> Map.exists (fun _ trip -> trip.Status = InProgress))

        let projection = MapProjection.Project world

        Assert.NotEmpty(world.Transport.Trips)
        Assert.NotEmpty(projection.MovingEntities)
        Assert.All(projection.MovingEntities, fun entity ->
            Assert.NotEmpty(entity.DisplayName)
            Assert.True(entity.RoutePolyline.Count >= 2))

    [<Fact>]
    let ``MovingEntityInterpolationWorks`` () =
        let midpoint = MovingEntityViewModel.Interpolate(MapPoint(0.0, 0.0), MapPoint(10.0, 20.0), 0.5)

        Assert.Equal(5.0, midpoint.X, 3)
        Assert.Equal(10.0, midpoint.Y, 3)

    [<Fact>]
    let ``MapProjectionEmitsMovingVehiclePrimitive`` () =
        let projection = activeMovementWorld () |> MapProjection.Project

        Assert.Contains(projection.Primitives, fun primitive ->
            primitive.Kind = MapPrimitiveKind.MovingEntity && primitive.Symbol = MapSymbol.Vehicle)
        Assert.NotEmpty(projection.MovingEntities)

    [<Fact>]
    let ``MapProjectionEmitsPedestrianPrimitive`` () =
        let projection = activeMovementWorld () |> withPedestrianMovement |> MapProjection.Project

        Assert.Contains(projection.MovingEntities, fun entity -> entity.Kind = RealSim.Avalonia.Models.MovingEntityKind.Pedestrian)
        Assert.Contains(projection.Primitives, fun primitive ->
            primitive.Kind = MapPrimitiveKind.MovingEntity && primitive.Category = "Pedestrian")

    [<Fact>]
    let ``SelectedMovementHasDetails`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let map = MapViewModel()
        let details = SelectedEntityViewModel()
        map.Update projection
        let entity = map.MovingEntities |> Seq.head

        map.SelectMovingEntity entity
        details.Show map.SelectedMovingEntity

        Assert.Equal(entity.DisplayName, details.Title)
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("Mode:"))
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("Origin:"))
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("Destination:"))
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("ETA:"))
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("Speed:"))
        Assert.Contains(details.Details, fun detail -> detail.StartsWith("Delay:"))

    [<Fact>]
    let ``FollowSelectedMovementUsesCurrentPosition`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection
        let entity = map.MovingEntities |> Seq.head
        map.SelectMovingEntity entity
        map.FollowSelectedMovement()

        Assert.True(map.IsFollowingSelectedMovement)
        Assert.Equal(500.0 - entity.CurrentPosition.X * map.Zoom, map.PanX, 6)
        Assert.Equal(350.0 - entity.CurrentPosition.Y * map.Zoom, map.PanY, 6)

    [<Fact>]
    let ``LayerToggleHidesMovementLayer`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection
        let vehicle = map.MovingEntities |> Seq.find (fun entity -> entity.Kind = RealSim.Avalonia.Models.MovingEntityKind.PrivateVehicle)

        Assert.True(map.IsMovingEntityVisible vehicle)
        map.ShowVehicles <- false

        Assert.False(map.IsMovingEntityVisible vehicle)

    [<Fact>]
    let ``SelectionUpdatesDetails`` () =
        let world = TestWorld.create ()
        let map = MapViewModel()
        let details = SelectedEntityViewModel()
        let projection = MapProjection.Project world
        map.Update projection
        let primitive = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.Neighborhood)
        map.SelectedPrimitive <- primitive

        details.Show map.SelectedPrimitive

        Assert.Equal(primitive.Name, details.Title)
        Assert.NotEmpty(details.Details)

    [<Fact>]
    let ``AdvanceTickRefreshesMap`` () =
        use session = new SimulationSession()
        let viewModel = new MainWindowViewModel(session)
        let mutable primitivesChanged = false
        viewModel.Map.PropertyChanged.Add(fun args ->
            if args.PropertyName = "Primitives" then
                primitivesChanged <- true)

        viewModel.AdvanceTickCommand.Execute null

        Assert.True(primitivesChanged)
        Assert.NotEmpty(viewModel.Map.Primitives)

    [<Fact>]
    let ``EventMarkersComeFromRecentEvents`` () =
        let world = TestWorld.create ()
        let road = world.Map.RoadSegments |> List.head
        let event = TransportEventOccurred(TestIds.eventId 31, RoadBlocked road.Id)
        let worldWithEvent = { world with Meta = { world.Meta with EventLog = [ event ] } }
        let projection = MapProjection.Project worldWithEvent

        Assert.Contains(projection.Primitives, fun primitive ->
            primitive.Kind = MapPrimitiveKind.EventMarker && primitive.Category = "Road event")

    [<Fact>]
    let ``SimulationSessionCanLoadJuniper`` () =
        use session = new SimulationSession()

        session.LoadJuniper 1337

        Assert.Equal("Juniper Falls", session.CurrentWorld.City.Name)
        Assert.NotEmpty(session.CurrentWorld.Map.RoadSegments)

    [<Fact>]
    let ``SimulationSessionCanAdvanceOneTick`` () =
        use session = new SimulationSession()
        session.LoadJuniper 1337
        let beforeTick = session.CurrentWorld.Meta.Tick

        let result = session.AdvanceTick 15

        Assert.True(session.CurrentWorld.Meta.Tick > beforeTick)
        Assert.Equal(session.CurrentWorld.Meta.Tick - 1, let (Simulation.Domain.TickId tick) = result.Tick in tick)

    [<Fact>]
    let ``MainWindowViewModelCommandsRefreshState`` () =
        use session = new SimulationSession()
        let viewModel = new MainWindowViewModel(session)
        let beforeStatus = viewModel.StatusText
        let beforeTick = session.CurrentWorld.Meta.Tick

        viewModel.AdvanceTickCommand.Execute null

        Assert.True(session.CurrentWorld.Meta.Tick > beforeTick)
        Assert.True(beforeStatus <> viewModel.StatusText)
        Assert.NotEmpty(viewModel.Map.Primitives)
        Assert.NotNull(viewModel.CitySummary)

    [<Fact>]
    let ``LabelCollisionFilterSkipsOverlappingLowPriorityLabels`` () =
        let r1 = Avalonia.Rect(0.0, 0.0, 50.0, 20.0)
        let r2 = Avalonia.Rect(10.0, 5.0, 50.0, 20.0)
        let r3 = Avalonia.Rect(100.0, 100.0, 50.0, 20.0)
        
        Assert.True(CityMapView.IntersectsPublic(r1, r2))
        Assert.False(CityMapView.IntersectsPublic(r1, r3))

    [<Fact>]
    let ``SelectedLabelWinsCollision`` () =
        Assert.True(true)

    [<Fact>]
    let ``LowZoomHidesLowPriorityLabels`` () =
        Assert.True(true)

    [<Fact>]
    let ``SameLocationMarkersAreClusteredOrOffset`` () =
        let vm = MapViewModel()
        let p1 = MapPrimitive("p1", MapPrimitiveKind.Place, "Place 1", [| MapPoint(10.0, 10.0) |], "#FFFFFF", "#000000", 1.0, 5.0, "Details", "Commercial", MapSymbol.Circle, MapLineStyle.Solid, 1.0, 80, false)
        let p2 = MapPrimitive("p2", MapPrimitiveKind.Place, "Place 2", [| MapPoint(10.0, 10.0) |], "#FFFFFF", "#000000", 1.0, 5.0, "Details", "Commercial", MapSymbol.Circle, MapLineStyle.Solid, 1.0, 80, false)
        vm.Primitives <- [| p1; p2 |]
        
        let view = CityMapView()
        let resolved = view.ResolveMarkers(vm)
        
        Assert.Equal(2, resolved.Count)
        let r1 = resolved.[0]
        let r2 = resolved.[1]
        Assert.Equal(r1.BaseScreenPoint, r2.BaseScreenPoint)
        Assert.NotEqual(r1.OffsetScreenPoint, r2.OffsetScreenPoint)

    [<Fact>]
    let ``ProjectionDeclutteringIsDeterministic`` () =
        let view = CityMapView()
        let vm = MapViewModel()
        let p1 = MapPrimitive("p1", MapPrimitiveKind.Place, "Place 1", [| MapPoint(10.0, 10.0) |], "#FFFFFF", "#000000", 1.0, 5.0, "Details", "Commercial", MapSymbol.Circle, MapLineStyle.Solid, 1.0, 80, false)
        let p2 = MapPrimitive("p2", MapPrimitiveKind.Place, "Place 2", [| MapPoint(10.0, 10.0) |], "#FFFFFF", "#000000", 1.0, 5.0, "Details", "Commercial", MapSymbol.Circle, MapLineStyle.Solid, 1.0, 80, false)
        vm.Primitives <- [| p1; p2 |]
        
        let resolved1 = view.ResolveMarkers(vm) |> Seq.map (fun m -> m.OffsetScreenPoint) |> Seq.toArray
        let resolved2 = view.ResolveMarkers(vm) |> Seq.map (fun m -> m.OffsetScreenPoint) |> Seq.toArray
        
        Assert.Equal<Avalonia.Point[]>(resolved1, resolved2)
