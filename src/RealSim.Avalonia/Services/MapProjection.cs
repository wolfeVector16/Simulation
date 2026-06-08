using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Microsoft.FSharp.Reflection;
using RealSim.Avalonia.Models;
using SimDomain = Simulation.Domain;

namespace RealSim.Avalonia.Services;

public static class MapProjection
{
    private const double CanvasWidth = 1000.0;
    private const double CanvasHeight = 700.0;
    private const double Padding = 48.0;
    private const int MaxMovingEntities = 140;
    private const int MaxEventMarkers = 36;

    public static MapProjectionResult Project(SimDomain.World world)
    {
        var primitives = new List<MapPrimitive>();
        var roadNodes = FSharpInterop.Pairs(world.Map.RoadNodes).ToDictionary(item => item.Key, item => item.Value);
        var places = FSharpInterop.Pairs(world.Map.Places).ToDictionary(item => item.Key, item => item.Value);
        var parcels = FSharpInterop.Pairs(world.City.Parcels).ToDictionary(item => item.Key, item => item.Value);
        var roadSegments = world.Map.RoadSegments.ToDictionary(segment => segment.Id, segment => segment);
        var rawPoints = new List<(double X, double Y)>();

        rawPoints.AddRange(roadNodes.Values.Select(node => (node.Position.X, node.Position.Y)));
        rawPoints.AddRange(places.Values.Select(place => (place.Position.X, place.Position.Y)));
        rawPoints.AddRange(parcels.Values.Select(parcel => (parcel.Position.X, parcel.Position.Y)));
        rawPoints.AddRange(world.Geography.Features.Select(feature => (feature.Center.X, feature.Center.Y)));

        var bounds = CreateBounds(rawPoints);

        MapPoint project(SimDomain.Coordinates point) => ProjectPoint(point.X, point.Y, bounds);
        MapPoint projectRaw(double x, double y) => ProjectPoint(x, y, bounds);

        var neighborhoodCenters = NeighborhoodCenters(world, projectRaw);

        primitives.AddRange(ProjectGeography(world, project));
        primitives.AddRange(ProjectNeighborhoods(world, neighborhoodCenters));
        primitives.AddRange(ProjectParcels(world, project));
        primitives.AddRange(ProjectRoads(world, roadNodes, project));
        primitives.AddRange(ProjectTransitRoutes(world, roadNodes, places, project));
        primitives.AddRange(ProjectPlaces(places, project));
        primitives.AddRange(ProjectInstitutions(world, places, project));
        primitives.AddRange(ProjectHouseholds(world, places, project));

        var trafficFrame = Simulation.TrafficVisualization.getTrafficFrame(world);
        var movingEntities = ProjectMovingEntities(world, trafficFrame, places, roadNodes, project).ToArray();
        primitives.AddRange(ProjectActiveRoutes(movingEntities));
        primitives.AddRange(ProjectMovingEntityPrimitives(movingEntities));
        primitives.AddRange(ProjectEventMarkers(world, places, roadNodes, roadSegments, neighborhoodCenters, project));

        var semanticPrimitives = primitives.Select(ApplyVisualGrammar).ToArray();

        return new MapProjectionResult(
            semanticPrimitives.OrderBy(primitive => primitive.Layer).ThenBy(primitive => primitive.Id).ToArray(),
            CanvasWidth,
            CanvasHeight,
            CreateLegend(),
            movingEntities);
    }

    private static (double MinX, double MinY, double SpanX, double SpanY) CreateBounds(IReadOnlyList<(double X, double Y)> points)
    {
        if (points.Count == 0)
        {
            return (0.0, 0.0, 1.0, 1.0);
        }

        var minX = points.Min(point => point.X);
        var maxX = points.Max(point => point.X);
        var minY = points.Min(point => point.Y);
        var maxY = points.Max(point => point.Y);
        return (minX, minY, Math.Max(1.0, maxX - minX), Math.Max(1.0, maxY - minY));
    }

    private static MapPoint ProjectPoint(double x, double y, (double MinX, double MinY, double SpanX, double SpanY) bounds)
    {
        var drawWidth = CanvasWidth - Padding * 2.0;
        var drawHeight = CanvasHeight - Padding * 2.0;
        var scale = Math.Min(drawWidth / bounds.SpanX, drawHeight / bounds.SpanY);
        var usedWidth = bounds.SpanX * scale;
        var usedHeight = bounds.SpanY * scale;
        var offsetX = Padding + (drawWidth - usedWidth) / 2.0;
        var offsetY = Padding + (drawHeight - usedHeight) / 2.0;

        return new MapPoint(
            offsetX + (x - bounds.MinX) * scale,
            CanvasHeight - offsetY - (y - bounds.MinY) * scale);
    }

    private static IReadOnlyDictionary<SimDomain.NeighborhoodId, MapPoint> NeighborhoodCenters(
        SimDomain.World world,
        Func<double, double, MapPoint> projectRaw)
    {
        var parcels = FSharpInterop.Pairs(world.City.Parcels).Select(item => item.Value).OrderBy(parcel => parcel.Id.ToString()).ToArray();
        var fallbackCenters = parcels.Length == 0
            ? new[] { projectRaw(0.0, 0.0) }
            : parcels.Select(parcel => projectRaw(parcel.Position.X, parcel.Position.Y)).ToArray();

        return FSharpInterop.Pairs(world.Neighborhoods)
            .OrderBy(item => item.Key.ToString())
            .Select((item, index) => (item.Key, Center: fallbackCenters[index % fallbackCenters.Length]))
            .ToDictionary(item => item.Key, item => item.Center);
    }

    private static IEnumerable<MapPrimitive> ProjectGeography(SimDomain.World world, Func<SimDomain.Coordinates, MapPoint> project)
    {
        foreach (var feature in world.Geography.Features.OrderBy(feature => feature.Name))
        {
            var center = project(feature.Center);
            var kind = feature.Kind.ToString();
            var radius = Math.Clamp(feature.RadiusMeters / Math.Max(1.0, world.Map.MetersPerMapUnit), 36.0, 140.0);

            yield return new MapPrimitive(
                Id: $"feature:{feature.Name}",
                Kind: MapPrimitiveKind.Geography,
                Name: feature.Name,
                Points: Rectangle(center, radius * 1.35, radius),
                Fill: GeographyFill(kind),
                Stroke: GeographyStroke(kind),
                Thickness: 0.6,
                Radius: 0.0,
                Details: $"{Readable(kind)} feature, amenity={feature.AmenityValue:0.00}, floodRisk={feature.FloodRisk:0.00}",
                Category: kind,
                Symbol: kind.Contains("Park", StringComparison.OrdinalIgnoreCase) || kind.Contains("Forest", StringComparison.OrdinalIgnoreCase)
                    ? MapSymbol.Tree
                    : MapSymbol.Polygon,
                LabelMinZoom: 1.2,
                LabelPriority: 80,
                IsApproximate: true);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectNeighborhoods(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.NeighborhoodId, MapPoint> centers)
    {
        foreach (var item in FSharpInterop.Pairs(world.Neighborhoods).OrderBy(item => item.Key.ToString()))
        {
            var center = centers[item.Key];
            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: MapPrimitiveKind.Neighborhood,
                Name: item.Value.Name,
                Points: Rectangle(center, 260, 180),
                Fill: "#00000000",
                Stroke: "#8DA1B680",
                Thickness: 0.45,
                Radius: 0.0,
                Details: $"population={item.Value.Residents.Count}, land value={item.Value.LandValue:0.00}, rent pressure={item.Value.RentPressure:0.00}, safety={item.Value.Safety:0.00}, transit={item.Value.TransitAccess:0.00}",
                Category: "Neighborhood",
                Symbol: MapSymbol.Polygon,
                LabelMinZoom: 0.0,
                LabelPriority: 12,
                IsApproximate: true);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectParcels(SimDomain.World world, Func<SimDomain.Coordinates, MapPoint> project)
    {
        var industrialSites = FSharpInterop.Pairs(world.City.IndustrialSites).ToDictionary(item => item.Key, item => item.Value);

        foreach (var item in FSharpInterop.Pairs(world.City.Parcels).OrderBy(item => item.Key.ToString()))
        {
            var parcel = item.Value;
            var center = OffsetForId(project(parcel.Position), item.Key.ToString(), 3.2);
            var building = parcel.Building?.Value;
            var hasBuilding = building is not null;
            industrialSites.TryGetValue(item.Key, out var industrialSite);
            var useKey = industrialSite is not null ? IndustrialProjectionCategory(industrialSite) : building?.Use.ToString() ?? parcel.Zone.ToString();
            var size = hasBuilding ? BuildingSize(useKey, parcel.Density.ToString()) : 10.0;
            var category = hasBuilding ? useKey : parcel.Zone.ToString();
            var symbol = hasBuilding ? BuildingSymbol(useKey) : MapSymbol.Square;
            var details = hasBuilding
                ? BuildingDetails(building!, parcel, industrialSite)
                : $"vacant parcel, zone={Readable(parcel.Zone.ToString())}, density={Readable(parcel.Density.ToString())}, land={parcel.LandValue:0.00}, road={parcel.RoadConnected}";

            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: hasBuilding ? MapPrimitiveKind.Building : MapPrimitiveKind.Parcel,
                Name: building?.Name ?? parcel.Name,
                Points: BuildingShape(center, size, size, symbol),
                Fill: hasBuilding ? BuildingFill(useKey, building!.Status.ToString()) : "#00000000",
                Stroke: hasBuilding ? BuildingStroke(useKey) : "#91A58E",
                Thickness: hasBuilding ? 0.8 : 0.4,
                Radius: 0.0,
                Details: details,
                Category: category,
                Symbol: symbol,
                LineStyle: hasBuilding ? MapLineStyle.Solid : MapLineStyle.Dotted,
                LabelMinZoom: hasBuilding ? 1.65 : 2.6,
                LabelPriority: 70);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectRoads(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        foreach (var segment in world.Map.RoadSegments.OrderBy(segment => segment.Id.ToString()))
        {
            if (!roadNodes.TryGetValue(segment.From, out var from) || !roadNodes.TryGetValue(segment.To, out var to))
            {
                continue;
            }

            var roadClass = segment.RoadClass.ToString();
            var congestion = TryGetValue(world.Transport.SegmentCongestion, segment.Id, 0.0);
            var blocked = !segment.CurrentIncidents.IsEmpty || segment.UnderConstruction || congestion >= 0.90;
            var lineStyle = RoadLineStyle(roadClass, segment.UnderConstruction);
            var stroke = blocked ? "#D00000" : RoadStroke(roadClass);

            yield return new MapPrimitive(
                Id: segment.Id.ToString(),
                Kind: MapPrimitiveKind.Road,
                Name: segment.Name,
                Points: new[] { project(from.Position), project(to.Position) },
                Fill: "#00000000",
                Stroke: stroke,
                Thickness: RoadThickness(roadClass),
                Radius: 0.0,
                Details: $"class={Readable(roadClass)}, speed={segment.SpeedKph:0} kph, lanes={segment.LaneIds.Length}, congestion={congestion:0.00}, sidewalk={segment.SidewalkQuality:0.00}, bike={Readable(segment.BikeFacility.ToString())}, incidents={segment.CurrentIncidents.Count}, construction={segment.UnderConstruction}",
                Category: roadClass,
                Symbol: MapSymbol.Polygon,
                LineStyle: lineStyle,
                LabelMinZoom: RoadThickness(roadClass) >= 3.5 ? 0.72 : 1.75,
                LabelPriority: RoadThickness(roadClass) >= 3.5 ? 20 : 95);

            if (congestion >= 0.35)
            {
                yield return new MapPrimitive(
                    Id: $"{segment.Id}:congestion",
                    Kind: MapPrimitiveKind.RoadStatus,
                    Name: segment.Name,
                    Points: new[] { project(from.Position), project(to.Position) },
                    Fill: "#00000000",
                    Stroke: CongestionStroke(congestion),
                    Thickness: Math.Max(1.2, RoadThickness(roadClass) - 1.0),
                    Radius: 0.0,
                    Details: $"congestion warning={congestion:0.00}",
                    Category: "Congestion",
                    Symbol: MapSymbol.Warning,
                    LineStyle: MapLineStyle.Dashed,
                    LabelMinZoom: 3.0,
                    LabelPriority: 160);
            }
        }
    }

    private static IEnumerable<MapPrimitive> ProjectTransitRoutes(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        var stops = FSharpInterop.Pairs(world.Transport.TransitStops).ToDictionary(item => item.Key, item => item.Value);

        foreach (var item in FSharpInterop.Pairs(world.Transport.TransitRoutes).OrderBy(item => item.Key.ToString()))
        {
            var route = item.Value;
            var points = new List<MapPoint>();

            foreach (var stopId in route.Stops)
            {
                if (!stops.TryGetValue(stopId, out var stop))
                {
                    continue;
                }

                if (stop.Place is not null && places.TryGetValue(stop.Place.Value, out var place))
                {
                    points.Add(project(place.Position));
                }
                else if (stop.Node is not null && roadNodes.TryGetValue(stop.Node.Value, out var node))
                {
                    points.Add(project(node.Position));
                }
                else
                {
                    points.Add(project(stop.Position));
                }
            }

            if (points.Count < 2)
            {
                continue;
            }

            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: MapPrimitiveKind.TransitRoute,
                Name: route.Name,
                Points: points,
                Fill: "#00000000",
                Stroke: TransitStroke(route.Mode.ToString()),
                Thickness: route.DedicatedRightOfWay ? 3.1 : 2.2,
                Radius: 0.0,
                Details: $"{Readable(route.Mode.ToString())}, every {route.HeadwayMinutes}m, reliability={route.Reliability:0.00}, crowding={route.Crowding:0.00}, dedicated right of way={route.DedicatedRightOfWay}",
                Category: route.Mode.ToString(),
                Symbol: MapSymbol.Bus,
                LineStyle: MapLineStyle.Dashed,
                LabelMinZoom: 0.85,
                LabelPriority: 28);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectPlaces(
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        foreach (var item in places.OrderBy(item => item.Key.ToString()))
        {
            var place = item.Value;
            var kind = place.Kind.ToString();
            var point = OffsetForId(project(place.Position), item.Key.ToString(), 6.0);
            var symbol = PlaceSymbol(kind);
            var points = kind == "Park"
                ? Rectangle(point, 34.0, 24.0)
                : SchematicFootprint(point, PlaceFootprintWidth(kind), PlaceFootprintHeight(kind), item.Key.ToString());

            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: MapPrimitiveKind.Place,
                Name: place.Name,
                Points: points,
                Fill: PlaceFill(kind),
                Stroke: PlaceStroke(kind),
                Thickness: kind == "Park" ? 0.8 : 1.0,
                Radius: 0.0,
                Details: $"type={Readable(kind)}, access={Readable(place.RoadAccess.ToString())}",
                Category: kind,
                Symbol: symbol,
                LabelMinZoom: kind is "School" or "Daycare" or "Civic" or "Park" ? 0.95 : 1.85,
                LabelPriority: kind is "School" or "Daycare" or "Civic" ? 35 : 86);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectInstitutions(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        foreach (var item in FSharpInterop.Pairs(world.Institutions).OrderBy(item => item.Key.ToString()))
        {
            var institution = item.Value;
            if (institution.Place is null || !places.TryGetValue(institution.Place.Value, out var place))
            {
                continue;
            }

            var kind = institution.Kind.ToString();
            var point = OffsetForId(project(place.Position), item.Key.ToString(), 9.0);

            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: MapPrimitiveKind.Institution,
                Name: institution.Name,
                Points: SymbolShape(point, 17.0, InstitutionSymbol(kind)),
                Fill: InstitutionFill(kind),
                Stroke: InstitutionStroke(kind),
                Thickness: 1.8,
                Radius: InstitutionSymbol(kind) == MapSymbol.Circle ? 8.0 : 0.0,
                Details: $"kind={Readable(kind)}, capacity={institution.Capacity}, used={Simulation.WorldIndexes.usedCapacity(item.Key, world)}, funding={institution.Funding:0}, quality={institution.Quality:0.00}, trust={institution.Trust:0.00}, backlog={institution.Backlog}, failure modes={string.Join(", ", institution.FailureModes.Select(mode => Readable(mode.ToString())))}",
                Category: kind,
                Symbol: InstitutionSymbol(kind),
                LabelMinZoom: 0.75,
                LabelPriority: 18);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectHouseholds(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        foreach (var item in FSharpInterop.Pairs(world.Households).OrderBy(item => item.Key.ToString()))
        {
            var household = item.Value;
            if (!places.TryGetValue(household.Home, out var place))
            {
                continue;
            }

            var point = OffsetForId(project(place.Position), item.Key.ToString(), 13.0);
            yield return new MapPrimitive(
                Id: item.Key.ToString(),
                Kind: MapPrimitiveKind.Household,
                Name: household.Name,
                Points: SymbolShape(point, 5.0, MapSymbol.House),
                Fill: "#FFD166",
                Stroke: "#7A4D00",
                Thickness: 1.0,
                Radius: 0.0,
                Details: $"members={household.Members.Count}, funds={household.Funds:0}, rent={FormatOptionDecimal(household.RentMonthly)}, stability={household.Stability:0.00}, food={household.FoodSecurity:0.00}, transport={household.TransportationAccess:0.00}",
                Category: "Household",
                Symbol: MapSymbol.House,
                LabelMinZoom: 2.2,
                LabelPriority: 115);
        }
    }

    private static IEnumerable<MovingEntityProjection> ProjectMovingEntities(
        SimDomain.World world,
        SimDomain.TrafficFrame trafficFrame,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        var sims = FSharpInterop.Pairs(world.Sims).ToDictionary(item => item.Key, item => item.Value);
        var trips = FSharpInterop.Pairs(world.Transport.Trips).ToDictionary(item => item.Key, item => item.Value);
        var entities = trafficFrame.MovingEntities
            .OrderBy(entity => entity.MovementId.ToString())
            .Take(MaxMovingEntities)
            .ToArray();

        foreach (var entity in entities)
        {
            var routePolyline = entity.RoutePreview
                .Select(project)
                .ToArray();

            if (routePolyline.Length < 2)
            {
                continue;
            }

            var mode = entity.Mode.ToString();
            var kind = MovingKindFromFrame(entity.EntityKind, mode);
            var purpose = "Trip";
            var origin = "Origin";
            var destination = "Destination";
            if (trips.TryGetValue(entity.TripId, out var trip))
            {
                purpose = Readable(trip.Purpose.ToString());
                origin = LocationLabel(trip.Origin, places, roadNodes);
                destination = LocationLabel(trip.Destination, places, roadNodes);
            }
            var eta = FormatSimTime(entity.TripId, world, entity.Progress, trips);

            var displayName = "Movement";
            if (TryOptionValue<SimDomain.SimId>(entity.SimId, out var simId) && sims.TryGetValue(simId, out var sim))
            {
                displayName = sim.Name;
            }
            else if (TryOptionValue<SimDomain.VehicleId>(entity.VehicleId, out var vehicleId))
            {
                displayName = vehicleId.ToString();
            }

            yield return new MovingEntityProjection(
                Id: entity.MovementId.ToString(),
                Kind: kind,
                CurrentPosition: project(entity.CurrentPosition),
                Destination: routePolyline[^1],
                RoutePolyline: routePolyline,
                Progress: entity.Progress,
                DisplayName: $"{displayName} - {Readable(mode)}",
                Mode: Readable(mode),
                Purpose: purpose,
                Status: Readable(entity.Status.ToString()),
                Origin: origin,
                DestinationName: destination,
                Eta: eta,
                SpeedKph: entity.SpeedKph,
                DelaySeconds: entity.DelaySeconds,
                IsApproximate: false);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectMovingEntityPrimitives(IEnumerable<MovingEntityProjection> movingEntities)
    {
        foreach (var entity in movingEntities)
        {
            yield return new MapPrimitive(
                Id: entity.Id,
                Kind: MapPrimitiveKind.MovingEntity,
                Name: entity.DisplayName,
                Points: new[] { entity.CurrentPosition },
                Fill: MovingFill(entity.Kind, entity.Status),
                Stroke: MovingStroke(entity.Kind, entity.Status),
                Thickness: entity.Status.Contains("Blocked", StringComparison.OrdinalIgnoreCase) ||
                           entity.Status.Contains("Delayed", StringComparison.OrdinalIgnoreCase)
                    ? 2.4
                    : 1.3,
                Radius: MovingRadius(entity.Kind),
                Details: $"mode={entity.Mode}, purpose={entity.Purpose}, speed={entity.SpeedKph:0.0} kph, delay={entity.DelaySeconds}s, status={entity.Status}",
                Category: entity.Kind.ToString(),
                Symbol: MovingSymbol(entity.Kind),
                LabelMinZoom: 2.1,
                LabelPriority: 25);
        }
    }

    private static IEnumerable<MapPrimitive> ProjectActiveRoutes(IEnumerable<MovingEntityProjection> movingEntities)
    {
        foreach (var entity in movingEntities)
        {
            if (entity.RoutePolyline.Count >= 2)
            {
                yield return new MapPrimitive(
                    Id: $"{entity.Id}:route",
                    Kind: MapPrimitiveKind.ActiveRoute,
                    Name: entity.DisplayName,
                    Points: entity.RoutePolyline,
                    Fill: "#00000000",
                    Stroke: entity.Kind is MovingEntityKind.Pedestrian or MovingEntityKind.Bike ? "#00A896" : "#118AB2",
                    Thickness: 1.5,
                    Radius: 0.0,
                    Details: $"{entity.Purpose}, mode={entity.Mode}, progress={entity.Progress:P0}{(entity.IsApproximate ? ", approximate route" : "")}",
                    Category: entity.Kind.ToString(),
                    Symbol: MapSymbol.Destination,
                    LineStyle: entity.IsApproximate ? MapLineStyle.Dotted : MapLineStyle.Solid,
                    LabelMinZoom: 3.0,
                    LabelPriority: 145,
                    IsApproximate: entity.IsApproximate);

                yield return new MapPrimitive(
                    Id: $"{entity.Id}:destination",
                    Kind: MapPrimitiveKind.Destination,
                    Name: "Destination",
                    Points: SymbolShape(entity.Destination, 8.0, MapSymbol.Destination),
                    Fill: "#FFD60A",
                    Stroke: "#5C4700",
                    Thickness: 1.2,
                    Radius: 0.0,
                    Details: $"destination for {entity.DisplayName}",
                    Category: "Destination",
                    Symbol: MapSymbol.Destination,
                    LabelMinZoom: 2.4,
                    LabelPriority: 150);
            }
        }
    }

    private static IEnumerable<MapPrimitive> ProjectEventMarkers(
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        IReadOnlyDictionary<SimDomain.RoadSegmentId, SimDomain.RoadSegment> roadSegments,
        IReadOnlyDictionary<SimDomain.NeighborhoodId, MapPoint> neighborhoodCenters,
        Func<SimDomain.Coordinates, MapPoint> project)
    {
        var index = 0;
        foreach (var domainEvent in world.Meta.EventLog.Take(MaxEventMarkers))
        {
            if (!TryEventLocation(domainEvent, world, places, roadNodes, roadSegments, neighborhoodCenters, project, out var location, out var category))
            {
                continue;
            }

            var caseName = UnionCaseName(domainEvent);
            var markerPoint = OffsetForId(location, $"{caseName}:{index}", 8.0);
            yield return new MapPrimitive(
                Id: $"event:{index}:{caseName}",
                Kind: MapPrimitiveKind.EventMarker,
                Name: Readable(caseName),
                Points: SymbolShape(markerPoint, 12.0, MapSymbol.Warning),
                Fill: EventFill(caseName),
                Stroke: "#2B1700",
                Thickness: 1.3,
                Radius: 0.0,
                Details: EventFormatter.Format(domainEvent),
                Category: category,
                Symbol: MapSymbol.Warning,
                LabelMinZoom: 1.45,
                LabelPriority: 45);

            index++;
        }
    }

    private static bool TryLocationPoint(
        object locationRef,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        Func<SimDomain.Coordinates, MapPoint> project,
        out MapPoint point)
    {
        point = default;
        var (caseName, fields) = UnionCase(locationRef);
        if (caseName == "PlaceRef" && fields.Length > 0 && fields[0] is SimDomain.PlaceId placeId && places.TryGetValue(placeId, out var place))
        {
            point = project(place.Position);
            return true;
        }

        if (caseName == "NodeRef" && fields.Length > 0 && fields[0] is SimDomain.RoadNodeId nodeId && roadNodes.TryGetValue(nodeId, out var node))
        {
            point = project(node.Position);
            return true;
        }

        return false;
    }

    private static string LocationLabel(
        object locationRef,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes)
    {
        var (caseName, fields) = UnionCase(locationRef);
        if (caseName == "PlaceRef" && fields.Length > 0 && fields[0] is SimDomain.PlaceId placeId)
        {
            return places.TryGetValue(placeId, out var place) ? place.Name : placeId.ToString();
        }

        if (caseName == "NodeRef" && fields.Length > 0 && fields[0] is SimDomain.RoadNodeId nodeId)
        {
            return roadNodes.ContainsKey(nodeId) ? $"Road node {nodeId}" : nodeId.ToString();
        }

        if (caseName == "StopRef" && fields.Length > 0)
        {
            return $"Stop {fields[0]}";
        }

        if (caseName == "ParkingRef" && fields.Length > 0)
        {
            return $"Parking {fields[0]}";
        }

        return Readable(caseName);
    }

    private static string FormatSimTime(
        SimDomain.TransportTripId tripId,
        SimDomain.World world,
        double progress,
        IReadOnlyDictionary<SimDomain.TransportTripId, SimDomain.TransportTrip> trips)
    {
        if (!trips.TryGetValue(tripId, out var trip))
        {
            return "n/a";
        }

        var route = trip.CurrentRoute?.Value ?? trip.PlannedRoute?.Value;
        if (route is null)
        {
            return "n/a";
        }

        var remaining = Math.Max(0.0, 1.0 - Math.Clamp(progress, 0.0, 1.0));
        var etaMinute = world.MinuteOfDay + (int)Math.Ceiling(route.ExpectedMinutes * remaining);
        var dayOffset = etaMinute / 1440;
        var minuteOfDay = ((etaMinute % 1440) + 1440) % 1440;
        var hour = minuteOfDay / 60;
        var minute = minuteOfDay % 60;
        return dayOffset == 0
            ? $"{hour:00}:{minute:00}"
            : $"day {world.Day + dayOffset}, {hour:00}:{minute:00}";
    }

    private static bool TryEventLocation(
        SimDomain.DomainEvent domainEvent,
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        IReadOnlyDictionary<SimDomain.RoadSegmentId, SimDomain.RoadSegment> roadSegments,
        IReadOnlyDictionary<SimDomain.NeighborhoodId, MapPoint> neighborhoodCenters,
        Func<SimDomain.Coordinates, MapPoint> project,
        out MapPoint location,
        out string category)
    {
        var foundLocation = default(MapPoint);
        var foundCategory = "Event";
        var (caseName, fields) = UnionCase(domainEvent);

        bool placeFromField(int index)
        {
            if (fields.Length > index && fields[index] is SimDomain.PlaceId placeId && places.TryGetValue(placeId, out var place))
            {
                foundLocation = project(place.Position);
                foundCategory = "Place event";
                return true;
            }

            return false;
        }

        bool roadFromField(int index)
        {
            if (fields.Length > index && fields[index] is SimDomain.RoadSegmentId segmentId && TryRoadMidpoint(segmentId, roadSegments, roadNodes, project, out var roadLocation))
            {
                foundLocation = roadLocation;
                foundCategory = "Road event";
                return true;
            }

            return false;
        }

        bool institutionFromField(int index)
        {
            if (fields.Length > index && fields[index] is SimDomain.InstitutionId institutionId)
            {
                var institutions = FSharpInterop.Pairs(world.Institutions).ToDictionary(item => item.Key, item => item.Value);
                if (institutions.TryGetValue(institutionId, out var institution) &&
                    institution.Place is not null &&
                    places.TryGetValue(institution.Place.Value, out var place))
                {
                    foundLocation = project(place.Position);
                    foundCategory = "Institution event";
                    return true;
                }
            }

            return false;
        }

        bool neighborhoodFromField(int index)
        {
            if (fields.Length > index && fields[index] is SimDomain.NeighborhoodId neighborhoodId && neighborhoodCenters.TryGetValue(neighborhoodId, out var neighborhoodLocation))
            {
                foundLocation = neighborhoodLocation;
                foundCategory = "Neighborhood event";
                return true;
            }

            return false;
        }

        var matched = caseName switch
        {
            "PersonMoved" => placeFromField(3),
            "JobStarted" => placeFromField(2),
            "BusinessOpened" or "BusinessClosed" => placeFromField(1),
            "DeliveryDelayed" => placeFromField(1),
            "ServiceCapacityChanged" or "InstitutionCapacityChanged" or "InstitutionClosed" => institutionFromField(1),
            "InstitutionOpened" => fields.Length > 2 && fields[2] is SimDomain.Institution opened && opened.Place is not null && places.TryGetValue(opened.Place.Value, out var openedPlace) && SetFoundLocation(project(openedPlace.Position), "Institution event", ref foundLocation, ref foundCategory),
            "CrimeOccurred" or "NeighborhoodReputationChanged" => neighborhoodFromField(1),
            "RoadDamaged" or "RoadClosed" or "RoadReopened" => roadFromField(2),
            "RoadModified" or "RoadDestroyed" or "LaneConfigurationChanged" => roadFromField(2),
            "BuildingConstructed" or "BuildingDestroyed" => ParcelFromField(fields, 3, world, project, out foundLocation, out foundCategory),
            "ParcelZoned" or "ParcelRezoned" => ParcelFromField(fields, 2, world, project, out foundLocation, out foundCategory),
            "TransportEventOccurred" => fields.Length > 1 && TryTransportEventLocation(fields[1], world, places, roadNodes, roadSegments, project, out foundLocation, out foundCategory),
            _ => false
        };

        location = foundLocation;
        category = foundCategory;
        return matched;
    }

    private static bool TryTransportEventLocation(
        object transportEvent,
        SimDomain.World world,
        IReadOnlyDictionary<SimDomain.PlaceId, SimDomain.Place> places,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        IReadOnlyDictionary<SimDomain.RoadSegmentId, SimDomain.RoadSegment> roadSegments,
        Func<SimDomain.Coordinates, MapPoint> project,
        out MapPoint location,
        out string category)
    {
        location = default;
        category = "Transport event";
        var (caseName, fields) = UnionCase(transportEvent);

        if ((caseName is "CrashOccurred" or "RoadBlocked" or "ConstructionStarted" or "ConstructionEnded" or "RoadConditionDeclined") &&
            fields.Length > 0 &&
            fields[0] is SimDomain.RoadSegmentId segmentId &&
            TryRoadMidpoint(segmentId, roadSegments, roadNodes, project, out location))
        {
            category = "Road event";
            return true;
        }

        if ((caseName is "BikeCrashOccurred" or "PedestrianNearMissOccurred") &&
            fields.Length > 1 &&
            TryOptionValue<SimDomain.RoadSegmentId>(fields[1], out var optionalSegment) &&
            TryRoadMidpoint(optionalSegment, roadSegments, roadNodes, project, out location))
        {
            category = "Road event";
            return true;
        }

        if ((caseName is "TripDelayed" or "ParkingFailed" or "ParkingSearchStarted" or "IllegalParkingOccurred") &&
            fields.Length > 0 &&
            fields[0] is SimDomain.TransportTripId tripId)
        {
            var trips = FSharpInterop.Pairs(world.Transport.Trips).ToDictionary(item => item.Key, item => item.Value);
            if (trips.TryGetValue(tripId, out var trip) && TryLocationPoint(trip.Destination, places, roadNodes, project, out location))
            {
                category = "Trip event";
                return true;
            }
        }

        if (caseName == "EmergencyResponseDelayed" && fields.Length > 0 && fields[0] is SimDomain.InstitutionId institutionId)
        {
            var institutions = FSharpInterop.Pairs(world.Institutions).ToDictionary(item => item.Key, item => item.Value);
            if (institutions.TryGetValue(institutionId, out var institution) &&
                institution.Place is not null &&
                places.TryGetValue(institution.Place.Value, out var place))
            {
                location = project(place.Position);
                category = "Institution event";
                return true;
            }
        }

        if (caseName == "DeliveryDelayed" && fields.Length > 0 && fields[0] is SimDomain.PlaceId placeId && places.TryGetValue(placeId, out var delayedPlace))
        {
            location = project(delayedPlace.Position);
            category = "Place event";
            return true;
        }

        return false;
    }

    private static bool ParcelFromField(
        object[] fields,
        int index,
        SimDomain.World world,
        Func<SimDomain.Coordinates, MapPoint> project,
        out MapPoint location,
        out string category)
    {
        location = default;
        category = "Parcel event";
        if (fields.Length <= index || fields[index] is not SimDomain.ParcelId parcelId)
        {
            return false;
        }

        var parcels = FSharpInterop.Pairs(world.City.Parcels).ToDictionary(item => item.Key, item => item.Value);
        if (!parcels.TryGetValue(parcelId, out var parcel))
        {
            return false;
        }

        location = project(parcel.Position);
        return true;
    }

    private static bool TryRoadMidpoint(
        SimDomain.RoadSegmentId segmentId,
        IReadOnlyDictionary<SimDomain.RoadSegmentId, SimDomain.RoadSegment> roadSegments,
        IReadOnlyDictionary<SimDomain.RoadNodeId, SimDomain.RoadNode> roadNodes,
        Func<SimDomain.Coordinates, MapPoint> project,
        out MapPoint point)
    {
        point = default;
        if (!roadSegments.TryGetValue(segmentId, out var segment) ||
            !roadNodes.TryGetValue(segment.From, out var from) ||
            !roadNodes.TryGetValue(segment.To, out var to))
        {
            return false;
        }

        var a = project(from.Position);
        var b = project(to.Position);
        point = new MapPoint((a.X + b.X) / 2.0, (a.Y + b.Y) / 2.0);
        return true;
    }

    private static IReadOnlyList<MapPoint> Rectangle(MapPoint center, double width, double height)
    {
        var halfWidth = width / 2.0;
        var halfHeight = height / 2.0;
        return new[]
        {
            new MapPoint(center.X - halfWidth, center.Y - halfHeight),
            new MapPoint(center.X + halfWidth, center.Y - halfHeight),
            new MapPoint(center.X + halfWidth, center.Y + halfHeight),
            new MapPoint(center.X - halfWidth, center.Y + halfHeight)
        };
    }

    private static IReadOnlyList<MapPoint> BuildingShape(MapPoint center, double width, double height, MapSymbol symbol)
    {
        return symbol == MapSymbol.House
            ? new[]
            {
                new MapPoint(center.X - width / 2.0, center.Y + height / 3.0),
                new MapPoint(center.X - width / 2.0, center.Y - height / 6.0),
                new MapPoint(center.X, center.Y - height / 2.0),
                new MapPoint(center.X + width / 2.0, center.Y - height / 6.0),
                new MapPoint(center.X + width / 2.0, center.Y + height / 3.0)
            }
            : Rectangle(center, width, height);
    }

    private static IReadOnlyList<MapPoint> SchematicFootprint(MapPoint center, double width, double height, string id)
    {
        var snapped = SnapToGrid(OffsetForId(center, $"footprint:{id}", 4.0), 6.0);
        return Rectangle(snapped, width, height);
    }

    private static MapPoint SnapToGrid(MapPoint point, double grid)
    {
        return new MapPoint(Math.Round(point.X / grid) * grid, Math.Round(point.Y / grid) * grid);
    }

    private static IReadOnlyList<MapPoint> SymbolShape(MapPoint center, double size, MapSymbol symbol)
    {
        return symbol switch
        {
            MapSymbol.Diamond or MapSymbol.Destination => new[]
            {
                new MapPoint(center.X, center.Y - size),
                new MapPoint(center.X + size, center.Y),
                new MapPoint(center.X, center.Y + size),
                new MapPoint(center.X - size, center.Y)
            },
            MapSymbol.House => BuildingShape(center, size * 1.8, size * 1.6, MapSymbol.House),
            MapSymbol.Circle or MapSymbol.Tree => new[] { center },
            _ => Rectangle(center, size * 1.6, size * 1.6)
        };
    }

    private static IReadOnlyList<MapPoint> Ellipse(MapPoint center, double width, double height, int segments)
    {
        return Enumerable.Range(0, segments)
            .Select(index =>
            {
                var angle = Math.PI * 2.0 * index / segments;
                return new MapPoint(center.X + Math.Cos(angle) * width / 2.0, center.Y + Math.Sin(angle) * height / 2.0);
            })
            .ToArray();
    }

    private static MapPoint OffsetForId(MapPoint point, string id, double radius)
    {
        var hash = 17;
        foreach (var ch in id)
        {
            hash = unchecked(hash * 31 + ch);
        }

        var angle = (Math.Abs(hash) % 360) * Math.PI / 180.0;
        var distance = radius * (0.35 + (Math.Abs(hash / 360) % 100) / 100.0);
        return new MapPoint(point.X + Math.Cos(angle) * distance, point.Y + Math.Sin(angle) * distance);
    }

    private static double Distance(MapPoint a, MapPoint b)
    {
        return Math.Sqrt(Math.Pow(a.X - b.X, 2.0) + Math.Pow(a.Y - b.Y, 2.0));
    }

    private static bool SetFoundLocation(MapPoint point, string newCategory, ref MapPoint location, ref string category)
    {
        location = point;
        category = newCategory;
        return true;
    }

    private static T TryGetValue<TKey, T>(Microsoft.FSharp.Collections.FSharpMap<TKey, T> map, TKey key, T fallback)
        where TKey : notnull
    {
        foreach (var item in map)
        {
            if (EqualityComparer<TKey>.Default.Equals(item.Key, key))
            {
                return item.Value;
            }
        }

        return fallback;
    }

    private static bool IsUnionCase(object value, string caseName)
    {
        return UnionCaseName(value) == caseName;
    }

    private static bool TryUnionFields(object value, string caseName, out object[] fields)
    {
        var union = UnionCase(value);
        fields = union.Fields;
        return union.CaseName == caseName;
    }

    private static string UnionCaseName(object value)
    {
        return UnionCase(value).CaseName;
    }

    private static (string CaseName, object[] Fields) UnionCase(object value)
    {
        var (caseInfo, fields) = FSharpValue.GetUnionFields(value, value.GetType(), null);
        return (caseInfo.Name, fields);
    }

    private static bool TryOptionValue<T>(object? option, out T value)
    {
        value = default!;
        if (option is null)
        {
            return false;
        }

        var property = option.GetType().GetProperty("Value");
        if (property?.GetValue(option) is T typed)
        {
            value = typed;
            return true;
        }

        return false;
    }

    private static string FormatOptionDecimal(object? option)
    {
        return TryOptionValue<decimal>(option, out var value)
            ? value.ToString("0", CultureInfo.InvariantCulture)
            : "n/a";
    }

    private static string Readable(string text)
    {
        if (string.IsNullOrWhiteSpace(text))
        {
            return text;
        }

        var chars = new List<char> { text[0] };
        for (var i = 1; i < text.Length; i++)
        {
            if (char.IsUpper(text[i]) && !char.IsWhiteSpace(text[i - 1]))
            {
                chars.Add(' ');
            }

            chars.Add(text[i]);
        }

        return new string(chars.ToArray());
    }

    private static string GeographyFill(string kind)
    {
        return kind switch
        {
            "River" or "Lake" or "Coastline" or "Wetland" => "#82C7E633",
            "Forest" or "Parkland" => "#6DBB7630",
            "Floodplain" => "#B6D7A824",
            _ => "#E6D8AD22"
        };
    }

    private static string GeographyStroke(string kind)
    {
        return kind switch
        {
            "River" or "Lake" or "Coastline" or "Wetland" => "#3B8EA5",
            "Forest" or "Parkland" => "#3D7D44",
            _ => "#8E806A"
        };
    }

    private static string BuildingFill(string use, string status)
    {
        if (status is "Vacant" or "Abandoned")
        {
            return "#FFFFFF30";
        }

        return use switch
        {
            "Housing" => "#8ECAE6",
            "Commerce" => "#FFD166",
            "Industry" => "#B08968",
            "Warehouse/logistics" => "#B8A070",
            "Workshop/light production" => "#C9A227",
            "Light manufacturing" => "#A7C957",
            "Clean/flex industrial" => "#80CED7",
            "Industrial yard" => "#A98467",
            "Heavy industry" => "#7F5539",
            "Hazardous industry" => "#8D0801",
            "Extractive industry" => "#6F4E37",
            "Waste management" => "#6B705C",
            "Utility/power" => "#577590",
            "PublicService" => "#90DBF4",
            "Recreation" => "#95D5B2",
            _ => "#D0D5DD"
        };
    }

    private static string BuildingStroke(string use)
    {
        return use switch
        {
            "Housing" => "#1F5F7A",
            "Commerce" => "#8A5A00",
            "Industry" => "#5D4037",
            "Warehouse/logistics" => "#6C584C",
            "Workshop/light production" => "#7A5C00",
            "Light manufacturing" => "#386641",
            "Clean/flex industrial" => "#006D77",
            "Industrial yard" => "#5D4037",
            "Heavy industry" => "#3E2723",
            "Hazardous industry" => "#3D0000",
            "Extractive industry" => "#2F2118",
            "Waste management" => "#343A40",
            "Utility/power" => "#1D3557",
            "PublicService" => "#0B4F6C",
            "Recreation" => "#2D6A4F",
            _ => "#303030"
        };
    }

    private static double BuildingSize(string use, string density)
    {
        var size = use switch
        {
            "Housing" => 12.0,
            "Commerce" => 15.0,
            "Industry" => 20.0,
            "Warehouse/logistics" => 23.0,
            "Workshop/light production" => 14.0,
            "Light manufacturing" => 18.0,
            "Clean/flex industrial" => 17.0,
            "Industrial yard" => 19.0,
            "Heavy industry" => 22.0,
            "Hazardous industry" => 22.0,
            "Extractive industry" => 24.0,
            "Waste management" => 21.0,
            "Utility/power" => 19.0,
            "PublicService" => 16.0,
            "Recreation" => 14.0,
            _ => 12.0
        };

        return density switch
        {
            "HighDensity" => size + 5.0,
            "MediumDensity" => size + 2.0,
            _ => size
        };
    }

    private static MapSymbol BuildingSymbol(string use)
    {
        return use switch
        {
            "Housing" => MapSymbol.House,
            "Commerce" => MapSymbol.Storefront,
            "Industry" => MapSymbol.Warehouse,
            "Warehouse/logistics" => MapSymbol.Warehouse,
            "Workshop/light production" => MapSymbol.Storefront,
            "Light manufacturing" => MapSymbol.Warehouse,
            "Clean/flex industrial" => MapSymbol.Civic,
            "Industrial yard" => MapSymbol.Square,
            "Heavy industry" => MapSymbol.Warehouse,
            "Hazardous industry" => MapSymbol.Warning,
            "Extractive industry" => MapSymbol.Diamond,
            "Waste management" => MapSymbol.Warning,
            "Utility/power" => MapSymbol.Utility,
            "PublicService" => MapSymbol.Civic,
            "Recreation" => MapSymbol.Tree,
            _ => MapSymbol.Square
        };
    }

    private static string IndustrialProjectionCategory(SimDomain.IndustrialSite site)
    {
        var use = site.Use.ToString();

        if (use is "Warehouse" or "DistributionCenter" or "LastMileLogistics")
        {
            return "Warehouse/logistics";
        }

        if (use is "Workshop" or "MakerSpace" or "AutoRepair")
        {
            return "Workshop/light production";
        }

        if (use is "LightManufacturing" or "FoodProduction")
        {
            return "Light manufacturing";
        }

        if (use is "CleanManufacturing" or "ResearchAndDevelopmentFlex")
        {
            return "Clean/flex industrial";
        }

        if (use is "EquipmentYard" or "ContractorYard" or "StorageYard")
        {
            return "Industrial yard";
        }

        if (use is "ChemicalPlant" or "Refinery")
        {
            return "Hazardous industry";
        }

        if (use is "Mining" or "Quarry" or "CoalMine")
        {
            return "Extractive industry";
        }

        if (use is "Landfill" or "RecyclingCenter" or "WasteTransferStation")
        {
            return "Waste management";
        }

        if (use.StartsWith("PowerPlant", StringComparison.Ordinal))
        {
            return "Utility/power";
        }

        return "Heavy industry";
    }

    private static string BuildingDetails(SimDomain.Building building, SimDomain.Parcel parcel, SimDomain.IndustrialSite? industrialSite)
    {
        var baseDetails = $"use={Readable(building.Use.ToString())}, status={Readable(building.Status.ToString())}, capacity={building.Capacity}, occupants={building.Occupants}, jobs={building.Jobs}, parcel={parcel.Name}, zone={Readable(parcel.Zone.ToString())}, condition powered={parcel.Powered}, watered={parcel.Watered}, road={parcel.RoadConnected}";

        if (industrialSite is null)
        {
            return baseDetails;
        }

        var e = industrialSite.Externalities;
        var f = industrialSite.Freight;
        return $"{baseDetails}, industrial subtype={Readable(industrialSite.Use.ToString())}, form={Readable(industrialSite.Form.ToString())}, air={e.AirPollution:0.00}, ground={e.GroundPollution:0.00}, noise={e.Noise:0.00}, truck traffic={e.TruckTraffic:0.00}, inbound trucks/day={f.InboundTruckTripsPerDay:0}, outbound trucks/day={f.OutboundTruckTripsPerDay:0}";
    }

    private static string PlaceFill(string kind)
    {
        return kind switch
        {
            "Residence" => "#4CC9F0",
            "Workplace" => "#A2D2FF",
            "Commercial" => "#FFB703",
            "Industrial" or "Warehouse" => "#936639",
            "School" or "Daycare" => "#4895EF",
            "Park" => "#52B78866",
            "Civic" => "#72EFDD",
            "OutsideConnection" => "#ADB5BD",
            _ => "#CED4DA"
        };
    }

    private static string PlaceStroke(string kind)
    {
        return kind switch
        {
            "Commercial" => "#805B00",
            "Industrial" or "Warehouse" => "#4E342E",
            "School" or "Daycare" => "#063B7A",
            "Park" => "#1B4332",
            "Civic" => "#006466",
            _ => "#22333B"
        };
    }

    private static MapSymbol PlaceSymbol(string kind)
    {
        return kind switch
        {
            "Residence" => MapSymbol.House,
            "Commercial" => MapSymbol.Storefront,
            "Industrial" or "Warehouse" => MapSymbol.Warehouse,
            "School" or "Daycare" => MapSymbol.School,
            "Park" => MapSymbol.Tree,
            "Civic" => MapSymbol.Civic,
            "OutsideConnection" => MapSymbol.Diamond,
            _ => MapSymbol.Circle
        };
    }

    private static double PlaceRadius(string kind)
    {
        return kind switch
        {
            "Commercial" => 6.0,
            "Industrial" or "Warehouse" => 6.5,
            "School" or "Daycare" => 7.0,
            "Park" => 6.0,
            "Civic" => 6.0,
            _ => 4.0
        };
    }

    private static double PlaceFootprintWidth(string kind)
    {
        return kind switch
        {
            "Commercial" => 16.0,
            "Industrial" or "Warehouse" => 20.0,
            "School" or "Daycare" => 18.0,
            "Civic" => 18.0,
            "Residence" => 12.0,
            _ => 12.0
        };
    }

    private static double PlaceFootprintHeight(string kind)
    {
        return kind switch
        {
            "Commercial" => 12.0,
            "Industrial" or "Warehouse" => 16.0,
            "School" or "Daycare" => 14.0,
            "Civic" => 14.0,
            "Residence" => 10.0,
            _ => 10.0
        };
    }

    private static string InstitutionFill(string kind)
    {
        return kind switch
        {
            "HospitalInstitution" => "#EF476F",
            "PoliceInstitution" => "#2B6CB0",
            "SchoolInstitution" => "#118AB2",
            "TransitInstitution" => "#8338EC",
            "CourtInstitution" or "WelfareInstitution" => "#06D6A0",
            _ => "#73D2DE"
        };
    }

    private static string InstitutionStroke(string kind)
    {
        return kind switch
        {
            "HospitalInstitution" => "#7D102D",
            "PoliceInstitution" => "#0B2C52",
            "TransitInstitution" => "#3C096C",
            _ => "#063B4A"
        };
    }

    private static MapSymbol InstitutionSymbol(string kind)
    {
        return kind switch
        {
            "HospitalInstitution" => MapSymbol.Cross,
            "PoliceInstitution" => MapSymbol.Shield,
            "SchoolInstitution" => MapSymbol.School,
            "TransitInstitution" => MapSymbol.Bus,
            _ => MapSymbol.Civic
        };
    }

    private static string RoadStroke(string roadClass)
    {
        return roadClass switch
        {
            "Highway" or "Freeway" => "#24292F",
            "Arterial" => "#D77A00",
            "Collector" => "#6C757D",
            "IndustrialRoad" or "FreightCorridor" => "#7F5539",
            "LocalStreet" => "#ADB5BD",
            "Alley" or "ServiceRoad" => "#CED4DA",
            "PedestrianPath" => "#2A9D8F",
            "BikePath" => "#00A896",
            "TransitCorridor" or "TransitMall" => "#7B2CBF",
            _ => "#8D99AE"
        };
    }

    private static double RoadThickness(string roadClass)
    {
        return roadClass switch
        {
            "Highway" or "Freeway" => 5.4,
            "Arterial" => 4.0,
            "Collector" => 2.8,
            "IndustrialRoad" or "FreightCorridor" => 3.0,
            "TransitCorridor" or "TransitMall" => 2.8,
            "LocalStreet" => 1.8,
            "Alley" or "ServiceRoad" => 1.2,
            "PedestrianPath" or "BikePath" => 1.2,
            _ => 1.8
        };
    }

    private static MapLineStyle RoadLineStyle(string roadClass, bool underConstruction)
    {
        if (underConstruction)
        {
            return MapLineStyle.Dashed;
        }

        return roadClass switch
        {
            "PedestrianPath" => MapLineStyle.Dotted,
            "BikePath" => MapLineStyle.Dashed,
            _ => MapLineStyle.Solid
        };
    }

    private static string CongestionStroke(double congestion)
    {
        return congestion switch
        {
            >= 0.80 => "#D00000",
            >= 0.55 => "#F48C06",
            _ => "#FFBA08"
        };
    }

    private static string TransitStroke(string mode)
    {
        return mode switch
        {
            "Bus" or "SchoolBus" => "#8338EC",
            "Tram" => "#3A86FF",
            "Metro" => "#FF006E",
            _ => "#7B2CBF"
        };
    }

    private static string EventFill(string caseName)
    {
        return caseName.Contains("Road", StringComparison.OrdinalIgnoreCase) ||
               caseName.Contains("Crash", StringComparison.OrdinalIgnoreCase)
            ? "#F77F00"
            : caseName.Contains("Capacity", StringComparison.OrdinalIgnoreCase) ||
              caseName.Contains("Delayed", StringComparison.OrdinalIgnoreCase)
                ? "#FFD166"
                : "#EF476F";
    }

    private static MovingEntityKind MovingKindFromFrame(object entityKind, string mode)
    {
        var caseName = UnionCaseName(entityKind);
        var kindText = entityKind.ToString() ?? caseName;
        if (caseName == "Pedestrian" || kindText.Contains("Pedestrian", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.Pedestrian;
        }

        if (caseName == "Cyclist" || kindText.Contains("Cyclist", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.Bike;
        }

        if (caseName == "TransitVehicle" || kindText.Contains("TransitVehicle", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.Bus;
        }

        if (caseName == "EmergencyResponder" || kindText.Contains("EmergencyResponder", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.EmergencyVehicle;
        }

        if (caseName == "FreightVehicle" || kindText.Contains("FreightVehicle", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.FreightTruck;
        }

        if (caseName == "ServiceVehicle" || kindText.Contains("ServiceVehicle", StringComparison.OrdinalIgnoreCase))
        {
            return MovingEntityKind.ServiceVehicle;
        }

        return mode switch
        {
            "Bus" or "Tram" or "Metro" or "RegionalRail" or "SchoolBus" or "Paratransit" => MovingEntityKind.Bus,
            "FreightTruck" => MovingEntityKind.FreightTruck,
            "EmergencyVehicle" => MovingEntityKind.EmergencyVehicle,
            "ServiceVehicle" => MovingEntityKind.ServiceVehicle,
            "DeliveryVehicle" => MovingEntityKind.DeliveryVehicle,
            "PrivateCar" or "TaxiOrRideshare" => MovingEntityKind.PrivateVehicle,
            _ => MovingEntityKind.PrivateVehicle
        };
    }

    private static string MovingFill(MovingEntityKind kind, string status)
    {
        if (status.Contains("Blocked", StringComparison.OrdinalIgnoreCase) ||
            status.Contains("Failed", StringComparison.OrdinalIgnoreCase))
        {
            return "#EF476F";
        }

        if (status.Contains("Delayed", StringComparison.OrdinalIgnoreCase) ||
            status.Contains("Waiting", StringComparison.OrdinalIgnoreCase))
        {
            return "#FFD166";
        }

        return kind switch
        {
            MovingEntityKind.Pedestrian => "#FFFFFF",
            MovingEntityKind.Bike => "#06D6A0",
            MovingEntityKind.Bus => "#8338EC",
            MovingEntityKind.FreightTruck => "#7F5539",
            MovingEntityKind.EmergencyVehicle => "#EF476F",
            MovingEntityKind.ServiceVehicle or MovingEntityKind.DeliveryVehicle => "#F4A261",
            _ => "#118AB2"
        };
    }

    private static string MovingStroke(MovingEntityKind kind, string status)
    {
        if (status.Contains("Blocked", StringComparison.OrdinalIgnoreCase) ||
            status.Contains("Delayed", StringComparison.OrdinalIgnoreCase) ||
            status.Contains("Waiting", StringComparison.OrdinalIgnoreCase))
        {
            return "#2B1700";
        }

        return kind switch
        {
            MovingEntityKind.Pedestrian => "#111111",
            MovingEntityKind.Bike => "#004B3A",
            MovingEntityKind.Bus => "#3C096C",
            MovingEntityKind.EmergencyVehicle => "#7D102D",
            _ => "#073B4C"
        };
    }

    private static MapSymbol MovingSymbol(MovingEntityKind kind)
    {
        return kind switch
        {
            MovingEntityKind.Pedestrian or MovingEntityKind.Sim => MapSymbol.Person,
            MovingEntityKind.Bike => MapSymbol.Bike,
            MovingEntityKind.Bus => MapSymbol.Bus,
            MovingEntityKind.FreightTruck => MapSymbol.Truck,
            MovingEntityKind.EmergencyVehicle => MapSymbol.Emergency,
            _ => MapSymbol.Vehicle
        };
    }

    private static double MovingRadius(MovingEntityKind kind)
    {
        return kind switch
        {
            MovingEntityKind.Bus => 7.0,
            MovingEntityKind.FreightTruck => 7.5,
            MovingEntityKind.EmergencyVehicle => 7.0,
            MovingEntityKind.Pedestrian => 2.8,
            MovingEntityKind.Bike => 3.4,
            _ => 4.2
        };
    }

    private static IReadOnlyList<LegendItem> CreateLegend()
    {
        return new[]
        {
            new LegendItem("Road", "width shows road class", "#00000000", "#24292F", 5.5, MapSymbol.Polygon),
            new LegendItem("Highway road", "thick dark major road", "#00000000", "#24292F", 7.2, MapSymbol.Polygon),
            new LegendItem("Residential building", "blue footprint", "#8ECAE6", "#1F5F7A", 1.0, MapSymbol.Square),
            new LegendItem("Commercial building", "yellow footprint", "#FFD166", "#8A5A00", 1.0, MapSymbol.Square),
            new LegendItem("Industrial building", "brown footprint", "#B08968", "#5D4037", 1.0, MapSymbol.Square),
            new LegendItem("Civic/institution", "small marked footprint", "#4895EF", "#063B7A", 1.4, MapSymbol.School),
            new LegendItem("Vehicle", "small directional marker", "#118AB2", "#073B4C", 1.2, MapSymbol.Vehicle),
            new LegendItem("Pedestrian/sim", "tiny person dot", "#FFFFFF", "#111111", 1.0, MapSymbol.Person),
            new LegendItem("Transit", "dashed route and stops", "#00000000", "#8338EC", 2.2, MapSymbol.Bus, MapLineStyle.Dashed),
            new LegendItem("Park/open space", "subtle green terrain", "#52B78833", "#1B4332", 0.8, MapSymbol.Polygon),
            new LegendItem("Selected entity", "yellow ring and label", "#FFF3B0", "#FFD60A", 3.0, MapSymbol.Square),
            new LegendItem("Recent event/warning", "important marker", "#EF476F", "#2B1700", 1.3, MapSymbol.Warning)
        };
    }

    private static MapPrimitive ApplyVisualGrammar(MapPrimitive primitive)
    {
        var role = primitive.Kind switch
        {
            MapPrimitiveKind.Geography when primitive.Category.Contains("Park", StringComparison.OrdinalIgnoreCase) ||
                                           primitive.Category.Contains("Forest", StringComparison.OrdinalIgnoreCase) => primitive.IsApproximate ? VisualRole.Debug : VisualRole.Park,
            MapPrimitiveKind.Geography => VisualRole.Terrain,
            MapPrimitiveKind.Neighborhood => VisualRole.NeighborhoodBoundary,
            MapPrimitiveKind.Parcel or MapPrimitiveKind.Building => VisualRole.BuildingFootprint,
            MapPrimitiveKind.Road or MapPrimitiveKind.RoadStatus => VisualRole.Road,
            MapPrimitiveKind.TransitRoute or MapPrimitiveKind.ActiveRoute => VisualRole.TransitRoute,
            MapPrimitiveKind.Institution => VisualRole.InstitutionMarker,
            MapPrimitiveKind.MovingEntity when primitive.Symbol == MapSymbol.Person => VisualRole.Pedestrian,
            MapPrimitiveKind.MovingEntity => VisualRole.Vehicle,
            MapPrimitiveKind.EventMarker => VisualRole.Event,
            MapPrimitiveKind.Place when primitive.Category == "Park" => VisualRole.Park,
            MapPrimitiveKind.Household => VisualRole.Debug,
            _ => VisualRole.Debug
        };

        var geometry = primitive.Kind switch
        {
            MapPrimitiveKind.Road or MapPrimitiveKind.RoadStatus or MapPrimitiveKind.TransitRoute or MapPrimitiveKind.ActiveRoute => MapGeometryType.Line,
            MapPrimitiveKind.Building or MapPrimitiveKind.Parcel or MapPrimitiveKind.Institution => MapGeometryType.Footprint,
            MapPrimitiveKind.Place when primitive.Category == "Park" => MapGeometryType.Polygon,
            MapPrimitiveKind.Place => MapGeometryType.Footprint,
            MapPrimitiveKind.Household or MapPrimitiveKind.Destination or MapPrimitiveKind.EventMarker or MapPrimitiveKind.MovingEntity => MapGeometryType.Point,
            _ => MapGeometryType.Polygon
        };

        var layer = role switch
        {
            VisualRole.Terrain => 0,
            VisualRole.Park => 1,
            VisualRole.NeighborhoodBoundary => 2,
            VisualRole.BuildingFootprint => 3,
            VisualRole.Road => 4,
            VisualRole.TransitRoute => 5,
            VisualRole.Vehicle or VisualRole.Pedestrian => 6,
            VisualRole.InstitutionMarker => 7,
            VisualRole.Event => 8,
            VisualRole.Selection => 9,
            _ => 10
        };

        var clutter = primitive.Kind switch
        {
            MapPrimitiveKind.Household => MapClutterBehavior.Cluster,
            MapPrimitiveKind.EventMarker => MapClutterBehavior.HideWhenCrowded,
            MapPrimitiveKind.MovingEntity => MapClutterBehavior.Cluster,
            MapPrimitiveKind.Place => MapClutterBehavior.HideWhenCrowded,
            _ => MapClutterBehavior.Keep
        };

        return primitive with
        {
            VisualRole = role,
            Layer = layer,
            GeometryType = geometry,
            ClutterBehavior = clutter,
            SymbolRole = role.ToString(),
            IsSelectable = primitive.Kind != MapPrimitiveKind.RoadStatus
        };
    }
}
