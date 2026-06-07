using Avalonia;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.Controls;

public sealed class ResolvedMarker
{
    public string Id { get; init; } = "";
    public Point BaseScreenPoint { get; init; }
    public Point OffsetScreenPoint { get; init; }
    public double Radius { get; init; }
    public object Source { get; init; } = null!;
    public Rect Bounds => new(OffsetScreenPoint.X - Radius, OffsetScreenPoint.Y - Radius, Radius * 2.0, Radius * 2.0);
    public bool IsCluster { get; init; }
    public int Count { get; init; } = 1;
    public MapPrimitiveKind? ClusterKind { get; init; }
}
