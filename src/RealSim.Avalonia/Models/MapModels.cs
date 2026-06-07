using System;
using System.Collections.Generic;

namespace RealSim.Avalonia.Models;

public enum MapPrimitiveKind
{
    Geography,
    Neighborhood,
    Parcel,
    Road,
    RoadStatus,
    TransitRoute,
    ActiveRoute,
    Building,
    Institution,
    Household,
    Place,
    MovingEntity,
    Destination,
    EventMarker,
    Label
}

public enum MapSymbol
{
    Polygon,
    Circle,
    Square,
    Diamond,
    House,
    Storefront,
    Warehouse,
    School,
    Cross,
    Shield,
    Civic,
    Tree,
    Utility,
    Vehicle,
    Bus,
    Truck,
    Emergency,
    Person,
    Bike,
    Warning,
    Destination
}

public enum MapLineStyle
{
    Solid,
    Dashed,
    Dotted
}

public enum MovingEntityKind
{
    Sim,
    PrivateVehicle,
    Bus,
    FreightTruck,
    EmergencyVehicle,
    ServiceVehicle,
    DeliveryVehicle,
    Bike,
    Pedestrian
}

public readonly record struct MapPoint(double X, double Y);

public sealed record MapPrimitive(
    string Id,
    MapPrimitiveKind Kind,
    string Name,
    IReadOnlyList<MapPoint> Points,
    string Fill,
    string Stroke,
    double Thickness,
    double Radius,
    string Details,
    string Category = "",
    MapSymbol Symbol = MapSymbol.Polygon,
    MapLineStyle LineStyle = MapLineStyle.Solid,
    double LabelMinZoom = 1.0,
    int LabelPriority = 100,
    bool IsApproximate = false);

public sealed record LegendItem(
    string Label,
    string Description,
    string Fill,
    string Stroke,
    double Thickness,
    MapSymbol Symbol,
    MapLineStyle LineStyle = MapLineStyle.Solid);

public sealed record MovingEntityProjection(
    string Id,
    MovingEntityKind Kind,
    MapPoint CurrentPosition,
    MapPoint Destination,
    IReadOnlyList<MapPoint> RoutePolyline,
    double Progress,
    string DisplayName,
    string Mode,
    string Purpose,
    string Status,
    string Origin,
    string DestinationName,
    string Eta,
    double SpeedKph,
    int DelaySeconds,
    bool IsApproximate);

public sealed record MapProjectionResult(
    IReadOnlyList<MapPrimitive> Primitives,
    double Width,
    double Height,
    IReadOnlyList<LegendItem>? LegendItems = null,
    IReadOnlyList<MovingEntityProjection>? MovingEntities = null);

public sealed record EventLogItem(string Text);

public sealed record CitySummary(
    string Name,
    string PopulationText,
    string BudgetText,
    string DemandText,
    string DiagnosticsText);

public sealed record TransportSummary(
    string RoadText,
    string ReliabilityText,
    string EventsText);
