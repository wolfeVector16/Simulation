namespace RealSim.Tests

open Xunit
open Simulation.Domain
open RealSim.Avalonia.Models
open RealSim.Avalonia.Services
open RealSim.Avalonia.ViewModels

module AvaloniaViewerTests =
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
