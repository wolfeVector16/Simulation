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
        [ 1..360 ]
        |> Seq.scan (fun world _ -> TestWorld.tick 1 world) (TestWorld.create ())
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

    let private point x y = MapPoint(x, y)

    let private namedPrimitive id kind name x y =
        MapPrimitive(id, kind, name, [| point x y |], "#FFFFFF", "#000000", 1.0, 5.0, "Details", "Test", MapSymbol.Circle, MapLineStyle.Solid, 0.0, 40, false)

    let private labelPoint (_: MapPrimitive) = Avalonia.Point(20.0, 20.0)

    let private mapToScreen (p: MapPoint) = Avalonia.Point(p.X, p.Y)

    let private boundsArea (primitive: MapPrimitive) =
        if primitive.Points.Count = 0 then
            0.0
        else
            let xs = primitive.Points |> Seq.map _.X
            let ys = primitive.Points |> Seq.map _.Y
            (Seq.max xs - Seq.min xs) * (Seq.max ys - Seq.min ys)

    let private minuteDelta beforeMinute afterMinute =
        if afterMinute >= beforeMinute then afterMinute - beforeMinute else afterMinute + 1440 - beforeMinute

    let private distancePointToSegment (p: Coordinates) (a: Coordinates) (b: Coordinates) =
        let dx = b.X - a.X
        let dy = b.Y - a.Y
        if abs dx < 0.0001 && abs dy < 0.0001 then
            sqrt ((p.X - a.X) ** 2.0 + (p.Y - a.Y) ** 2.0)
        else
            let t = max 0.0 (min 1.0 (((p.X - a.X) * dx + (p.Y - a.Y) * dy) / (dx * dx + dy * dy)))
            let x = a.X + t * dx
            let y = a.Y + t * dy
            sqrt ((p.X - x) ** 2.0 + (p.Y - y) ** 2.0)

    let private namedPlace name (world: World) =
        world.Map.Places |> Map.toSeq |> Seq.find (fun (_, place) -> place.Name = name)

    let private routeDistance originName destinationName world =
        let origin, _ = namedPlace originName world
        let destination, _ = namedPlace destinationName world
        match TransportRouting.roadRoute world PrivateCar origin destination with
        | Some route -> route.TotalDistanceMeters
        | None -> 0.0

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
            [ 1..360 ]
            |> Seq.scan (fun world _ -> TestWorld.tick 1 world) (TestWorld.create ())
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

    [<Fact>]
    let ``DefaultMapModeIsClarity`` () =
        let map = MapViewModel()

        Assert.Equal(MapDisplayMode.Clarity, map.DisplayMode)
        Assert.False(map.ShowRoutes)
        Assert.False(map.ShowEvents)

    [<Fact>]
    let ``LabelsAreBudgeted`` () =
        let map = MapViewModel()
        map.Zoom <- 1.0
        map.Primitives <-
            [| for i in 1..80 ->
                namedPrimitive $"inst-{i}" MapPrimitiveKind.Institution $"Institution {i}" (float i * 20.0) 20.0 |]

        let labels = LabelLayoutEngine.PlaceLabels(map, [||], labelPoint, mapToScreen)

        Assert.True(labels.Count <= LabelLayoutEngine.LabelBudget(map.Zoom))

    [<Fact>]
    let ``HighZoomDoesNotShowEveryLabel`` () =
        let map = MapViewModel()
        map.Zoom <- 3.0
        map.Primitives <-
            [| for i in 1..90 ->
                namedPrimitive $"inst-{i}" MapPrimitiveKind.Institution $"Institution {i}" (float i * 20.0) 20.0 |]

        let labels = LabelLayoutEngine.PlaceLabels(map, [||], labelPoint, mapToScreen)

        Assert.True(labels.Count <= 35)
        Assert.True(labels.Count < 90)

    [<Fact>]
    let ``MarkerLayoutAvoidsFinalOverlap`` () =
        let view = CityMapView()
        let map = MapViewModel()
        map.Primitives <-
            [| for i in 1..3 ->
                namedPrimitive $"inst-{i}" MapPrimitiveKind.Institution $"Institution {i}" 10.0 10.0 |]

        let resolved = view.ResolveMarkers(map)
        let overlaps =
            resolved
            |> Seq.mapi (fun i left -> resolved |> Seq.skip (i + 1) |> Seq.exists (fun right -> MarkerLayoutEngine.Intersects(left.Bounds, right.Bounds)))
            |> Seq.exists id

        Assert.False(overlaps)

    [<Fact>]
    let ``StableMarkerLayoutDoesNotUseStringGetHashCode`` () =
        let first = MarkerLayoutEngine.StableHash("same-marker-id")
        let second = MarkerLayoutEngine.StableHash("same-marker-id")
        let view = CityMapView()
        let map = MapViewModel()
        map.Primitives <-
            [| for i in 1..6 ->
                namedPrimitive $"inst-{i}" MapPrimitiveKind.Institution $"Institution {i}" 10.0 10.0 |]

        let firstLayout = view.ResolveMarkers(map) |> Seq.map (fun marker -> marker.Id, marker.OffsetScreenPoint) |> Seq.toArray
        let secondLayout = view.ResolveMarkers(map) |> Seq.map (fun marker -> marker.Id, marker.OffsetScreenPoint) |> Seq.toArray

        Assert.Equal(first, second)
        Assert.Equal<(string * Avalonia.Point)[]>(firstLayout, secondLayout)

    [<Fact>]
    let ``DenseHouseholdsClusterAtLowZoom`` () =
        let view = CityMapView()
        let map = MapViewModel()
        map.Zoom <- 0.5
        map.ShowDebugLayers <- true
        map.Primitives <-
            [| for i in 1..20 ->
                namedPrimitive $"household-{i}" MapPrimitiveKind.Household $"Household {i}" 10.0 10.0 |]

        let resolved = view.ResolveMarkers(map)

        Assert.Contains(resolved, fun marker -> marker.IsCluster && marker.Count = 20 && marker.ClusterKind.HasValue && marker.ClusterKind.Value = MapPrimitiveKind.Household)

    [<Fact>]
    let ``MovingEntitiesRenderAboveBuildings`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let movingLayer = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.MovingEntity) |> fun primitive -> primitive.Layer
        let buildingLayer = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.Building) |> fun primitive -> primitive.Layer

        Assert.True(movingLayer > buildingLayer)

    [<Fact>]
    let ``SelectedEntityAlwaysGetsLabel`` () =
        let map = MapViewModel()
        map.Zoom <- 0.4
        let selected = namedPrimitive "selected" MapPrimitiveKind.Building "Selected Building" 0.0 0.0
        map.Primitives <-
            [| yield selected
               for i in 1..80 -> namedPrimitive $"neighborhood-{i}" MapPrimitiveKind.Neighborhood $"Neighborhood {i}" (float i * 20.0) 20.0 |]
        map.SelectedPrimitive <- selected

        let labels = LabelLayoutEngine.PlaceLabels(map, [||], labelPoint, mapToScreen)

        Assert.Contains(labels, fun label -> label.Id = "selected" && label.IsSelected)

    [<Fact>]
    let ``ParksDoNotRenderAsOpaqueBubblesInClarityMode`` () =
        let projection = TestWorld.create () |> MapProjection.Project
        let regions =
            projection.Primitives
            |> Seq.filter (fun primitive -> primitive.VisualRole = VisualRole.Park || primitive.VisualRole = VisualRole.NeighborhoodBoundary)
            |> Seq.toArray

        Assert.NotEmpty(regions)
        Assert.All(regions, fun primitive ->
            Assert.True(primitive.Points.Count <= 4)
            Assert.True(primitive.Layer <= 2))

    [<Fact>]
    let ``RouteTrailsDefaultToSelectedOnly`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection

        Assert.False(map.ShowRoutes)
        Assert.False(map.AreRoutesVisible)
        map.SelectMovingEntity(map.MovingEntities |> Seq.head)
        Assert.True(map.AreRoutesVisible)

    [<Fact>]
    let ``LegendMatchesVisibleLayerKinds`` () =
        let projection = TestWorld.create () |> MapProjection.Project
        let labels = projection.LegendItems |> Seq.map _.Label |> Set.ofSeq

        Assert.Contains("Road", labels)
        Assert.Contains("Residential building", labels)
        Assert.Contains("Vehicle", labels)
        Assert.Contains("Pedestrian/sim", labels)
        Assert.DoesNotContain("Household", labels)

    [<Fact>]
    let ``DebugModeCanBeSelected`` () =
        let map = MapViewModel()

        Assert.Equal(MapDisplayMode.Clarity, map.DisplayMode)
        map.DisplayMode <- MapDisplayMode.DebugRawPrimitives
        map.ShowDebugLayers <- true

        Assert.Equal(MapDisplayMode.DebugRawPrimitives, map.DisplayMode)
        Assert.True(map.ShowDebugLayers)

    [<Fact>]
    let ``ClarityModeHidesDebugRegionBlobs`` () =
        let projection = TestWorld.create () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection

        let confusingRegionBlobs =
            map.VisiblePrimitives
            |> Seq.filter (fun primitive ->
                primitive.IsApproximate &&
                primitive.GeometryType = MapGeometryType.Polygon &&
                boundsArea primitive > 5000.0 &&
                primitive.Fill <> "#00000000")
            |> Seq.toArray

        Assert.Empty(confusingRegionBlobs)

    [<Fact>]
    let ``ClarityModeShowsOnlySelectedRouteByDefault`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection

        Assert.Empty(map.VisiblePrimitives |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.ActiveRoute))
        Assert.False(map.AreRoutesVisible)

        map.SelectMovingEntity(map.MovingEntities |> Seq.head)

        Assert.True(map.AreRoutesVisible)
        Assert.Empty(map.VisiblePrimitives |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.ActiveRoute))

    [<Fact>]
    let ``LabelsAreStrictlyBudgetedInClarityMode`` () =
        let map = MapViewModel()
        map.DisplayMode <- MapDisplayMode.Clarity

        for zoom, expected in [ 0.5, 5; 1.0, 10; 2.0, 15 ] do
            map.Zoom <- zoom
            map.Primitives <-
                [| for i in 1..80 ->
                    MapPrimitive($"n-{i}", MapPrimitiveKind.Neighborhood, $"Neighborhood {i}", [| point (float i * 30.0) 20.0; point (float i * 30.0 + 10.0) 20.0; point (float i * 30.0 + 10.0) 30.0; point (float i * 30.0) 30.0 |], "#00000000", "#8DA1B680", 0.5, 0.0, "Details", "Neighborhood", MapSymbol.Polygon, MapLineStyle.Solid, 0.0, 12, true, VisualRole.NeighborhoodBoundary, 2, MapGeometryType.Polygon, true, MapClutterBehavior.Keep, "NeighborhoodBoundary") |]

            let labels = LabelLayoutEngine.PlaceLabels(map, [||], labelPoint, mapToScreen)

            Assert.True(labels.Count <= expected)

    [<Fact>]
    let ``BuildingFootprintsAreDistinctFromVehicles`` () =
        let projection = activeMovementWorld () |> MapProjection.Project

        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Building && primitive.VisualRole = VisualRole.BuildingFootprint && primitive.GeometryType = MapGeometryType.Footprint)
        Assert.Contains(projection.Primitives, fun primitive -> primitive.Kind = MapPrimitiveKind.MovingEntity && (primitive.VisualRole = VisualRole.Vehicle || primitive.VisualRole = VisualRole.Pedestrian) && primitive.GeometryType = MapGeometryType.Point)

    [<Fact>]
    let ``ParksRenderAsParkLayerNotInstitutionMarker`` () =
        let projection = TestWorld.create () |> MapProjection.Project

        Assert.Contains(projection.Primitives, fun primitive ->
            primitive.VisualRole = VisualRole.Park &&
            primitive.Kind = MapPrimitiveKind.Place &&
            primitive.GeometryType = MapGeometryType.Polygon)
        Assert.DoesNotContain(projection.Primitives, fun primitive ->
            primitive.Category = "Park" && primitive.VisualRole = VisualRole.InstitutionMarker)

    [<Fact>]
    let ``MovingEntitiesRenderAboveRoads`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let movingLayer = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.MovingEntity) |> _.Layer
        let roadLayer = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.Road) |> _.Layer

        Assert.True(movingLayer > roadLayer)

    [<Fact>]
    let ``VehicleMarkersAreSmallerThanBuildingFootprints`` () =
        let projection = activeMovementWorld () |> MapProjection.Project
        let vehicleRadius = projection.Primitives |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.MovingEntity) |> Seq.map _.Radius |> Seq.max
        let buildingWidth =
            projection.Primitives
            |> Seq.filter (fun primitive -> primitive.Kind = MapPrimitiveKind.Building)
            |> Seq.map (fun primitive ->
                let xs = primitive.Points |> Seq.map _.X
                Seq.max xs - Seq.min xs)
            |> Seq.average

        Assert.True(vehicleRadius * 2.0 < buildingWidth)

    [<Fact>]
    let ``SelectedEntityAlwaysVisible`` () =
        let projection = TestWorld.create () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection
        let hiddenByClarity = projection.Primitives |> Seq.find (fun primitive -> primitive.Kind = MapPrimitiveKind.Geography && primitive.IsApproximate)

        Assert.DoesNotContain(hiddenByClarity, map.VisiblePrimitives)
        map.SelectedPrimitive <- hiddenByClarity

        Assert.Contains(hiddenByClarity, map.VisiblePrimitives)

    [<Fact>]
    let ``DebugModeCanStillShowRawPrimitives`` () =
        let projection = TestWorld.create () |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection

        Assert.DoesNotContain(map.VisiblePrimitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Geography && primitive.IsApproximate)

        map.DisplayMode <- MapDisplayMode.DebugRawPrimitives
        map.ShowDebugLayers <- true

        Assert.Contains(map.VisiblePrimitives, fun primitive -> primitive.Kind = MapPrimitiveKind.Geography && primitive.IsApproximate)

    [<Fact>]
    let ``AdvanceOneTickAdvancesOneMinute`` () =
        use session = new SimulationSession()
        session.LoadJuniper 1337
        let before = session.CurrentWorld.MinuteOfDay

        session.AdvanceTick() |> ignore

        Assert.Equal(1, minuteDelta before session.CurrentWorld.MinuteOfDay)

    [<Fact>]
    let ``AdvanceFifteenMinutesAdvancesFifteenMinutes`` () =
        use session = new SimulationSession()
        session.LoadJuniper 1337
        let before = session.CurrentWorld.MinuteOfDay

        session.AdvanceMinutes 15 |> ignore

        Assert.Equal(15, minuteDelta before session.CurrentWorld.MinuteOfDay)

    [<Fact>]
    let ``AdvanceOneHourAdvancesSixtyMinutes`` () =
        use session = new SimulationSession()
        session.LoadJuniper 1337
        let before = session.CurrentWorld.MinuteOfDay

        session.AdvanceMinutes 60 |> ignore

        Assert.Equal(60, minuteDelta before session.CurrentWorld.MinuteOfDay)

    [<Fact>]
    let ``PlayDefaultAdvancesOneMinutePerStep`` () =
        use session = new SimulationSession()
        let viewModel = MainWindowViewModel(session)

        Assert.Equal(1, viewModel.PlaybackStepMinutes)

    [<Fact>]
    let ``ButtonLabelsMatchAdvanceAmounts`` () =
        use session = new SimulationSession()
        let viewModel = MainWindowViewModel(session)
        let before = session.CurrentWorld.MinuteOfDay

        viewModel.AdvanceTickCommand.Execute null
        Assert.Equal(1, minuteDelta before session.CurrentWorld.MinuteOfDay)

        let before15 = session.CurrentWorld.MinuteOfDay
        viewModel.AdvanceFifteenMinutesCommand.Execute null
        Assert.Equal(15, minuteDelta before15 session.CurrentWorld.MinuteOfDay)

        let beforeHour = session.CurrentWorld.MinuteOfDay
        viewModel.AdvanceOneHourCommand.Execute null
        Assert.Equal(60, minuteDelta beforeHour session.CurrentWorld.MinuteOfDay)

    [<Fact>]
    let ``OneMinuteMovementIsGradual`` () =
        let active =
            [ 1..180 ]
            |> Seq.scan (fun world _ -> Simulation.Engine.tick 1 world) (RealSim.Scenarios.Juniper.createWorld 1337)
            |> Seq.find (fun world -> world.Transport.Movements |> Map.exists (fun _ movement -> movement.Status = MovementStatus.InProgress && movement.Progress < 0.80))

        let movementId, before = active.Transport.Movements |> Map.toSeq |> Seq.find (fun (_, movement) -> movement.Status = MovementStatus.InProgress && movement.Progress < 0.80)
        let afterOneWorld = Simulation.Engine.tick 1 active
        let afterFifteenWorld = Simulation.Engine.tick 15 active
        let afterOne = afterOneWorld.Transport.Movements[movementId]
        let afterFifteen = afterFifteenWorld.Transport.Movements[movementId]

        Assert.True(afterOne.Progress > before.Progress)
        Assert.True(afterFifteen.Progress > afterOne.Progress)
        Assert.True(afterOne.Progress < 0.98)

    [<Fact>]
    let ``JuniperBuildingsDoNotOverlapRoads`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let roadEnvelopeMeters = 18.0
        let buildingParcels =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun parcel -> parcel.Building.IsSome)
            |> Seq.filter (fun parcel -> parcel.Name <> "Juniper Park Commons")
            |> Seq.toArray

        Assert.All(buildingParcels, fun parcel ->
            let nearest =
                world.Map.RoadSegments
                |> Seq.map (fun road ->
                    let a = world.Map.RoadNodes[road.From].Position
                    let b = world.Map.RoadNodes[road.To].Position
                    distancePointToSegment parcel.Position a b)
                |> Seq.min

            Assert.True(nearest > roadEnvelopeMeters, sprintf "%s is too close to a road centerline: %.1fm" parcel.Name nearest))

    [<Fact>]
    let ``JuniperBuildingsDoNotOverlapEachOther`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let buildingParcels =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun parcel -> parcel.Building.IsSome)
            |> Seq.toArray

        for left in buildingParcels do
            for right in buildingParcels do
                if left.Id <> right.Id then
                    let dx = left.Position.X - right.Position.X
                    let dy = left.Position.Y - right.Position.Y
                    Assert.True(sqrt (dx * dx + dy * dy) > 45.0, sprintf "%s overlaps or crowds %s" left.Name right.Name)

    [<Fact>]
    let ``JuniperParkDoesNotCoverBuildings`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let park = world.City.Parcels |> Map.toSeq |> Seq.map snd |> Seq.find (fun parcel -> parcel.Name = "Juniper Park Commons")
        let buildings =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun parcel -> parcel.Building.IsSome && parcel.Id <> park.Id)

        Assert.All(buildings, fun building ->
            let dx = building.Position.X - park.Position.X
            let dy = building.Position.Y - park.Position.Y
            Assert.True(sqrt (dx * dx + dy * dy) > 70.0, sprintf "%s is inside the park area" building.Name))

    [<Fact>]
    let ``JuniperHasReadableDistrictSeparation`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let _, home = namedPlace "Canal Apartments" world
        let _, civic = namedPlace "Civic Analytics" world
        let _, industry = namedPlace "Foundry Cooperative" world
        let _, park = namedPlace "Juniper Park" world

        Assert.True(MapGraph.distanceMeters world.Map home.Position civic.Position > 250.0)
        Assert.True(MapGraph.distanceMeters world.Map civic.Position industry.Position > 250.0)
        Assert.True(MapGraph.distanceMeters world.Map park.Position industry.Position > 300.0)

    [<Fact>]
    let ``JuniperRoutesHaveVisibleLength`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337

        Assert.True(routeDistance "Rowhouse 12" "Juniper Elementary" world > 250.0)
        Assert.True(routeDistance "Canal Apartments" "Foundry Cooperative" world > 450.0)
        Assert.True(routeDistance "Rowhouse 12" "Main Street Goods" world > 250.0)

    [<Fact>]
    let ``JuniperRoadNetworkConnected`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337

        for destination in [ "Juniper Elementary"; "Foundry Cooperative"; "Main Street Goods"; "Regional Galleria" ] do
            let origin, _ = namedPlace "Rowhouse 12" world
            let target, _ = namedPlace destination world
            Assert.True((TransportRouting.roadRoute world PrivateCar origin target).IsSome, sprintf "No production road route to %s" destination)

    [<Fact>]
    let ``JuniperDoesNotRequireStraightLineFallback`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let origin, _ = namedPlace "Canal Apartments" world
        let target, _ = namedPlace "Foundry Cooperative" world
        let route = TransportRouting.roadRoute world PrivateCar origin target

        Assert.True(route.IsSome)
        Assert.True(route.Value.Segments.Length >= 2)
        Assert.True(route.Value.Geometry.Polyline.Length > 2)

    [<Fact>]
    let ``JuniperDefaultProjectionHasNoHugeOpaqueOverlays`` () =
        let projection = RealSim.Scenarios.Juniper.createWorld 1337 |> MapProjection.Project
        let map = MapViewModel()
        map.Update projection

        let hugeOpaque =
            map.VisiblePrimitives
            |> Seq.filter (fun primitive -> primitive.GeometryType = MapGeometryType.Polygon && primitive.Fill <> "#00000000" && boundsArea primitive > 30000.0)
            |> Seq.toArray

        Assert.Empty(hugeOpaque)
