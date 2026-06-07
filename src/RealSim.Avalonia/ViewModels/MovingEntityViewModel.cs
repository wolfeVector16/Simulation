using System;
using System.Collections.Generic;
using CommunityToolkit.Mvvm.ComponentModel;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class MovingEntityViewModel : ObservableObject
{
    private static readonly TimeSpan AnimationDuration = TimeSpan.FromMilliseconds(900);

    public MovingEntityViewModel(MovingEntityProjection projection)
    {
        Id = projection.Id;
        Kind = projection.Kind;
        DisplayName = projection.DisplayName;
        Mode = projection.Mode;
        Purpose = projection.Purpose;
        Status = projection.Status;
        PreviousPosition = projection.CurrentPosition;
        CurrentPosition = projection.CurrentPosition;
        Destination = projection.Destination;
        RoutePolyline = projection.RoutePolyline;
        Progress = projection.Progress;
        IsApproximate = projection.IsApproximate;
        LastUpdatedUtc = DateTime.UtcNow;
    }

    public string Id { get; }

    public MovingEntityKind Kind { get; }

    [ObservableProperty]
    private MapPoint previousPosition;

    [ObservableProperty]
    private MapPoint currentPosition;

    [ObservableProperty]
    private MapPoint destination;

    [ObservableProperty]
    private IReadOnlyList<MapPoint> routePolyline = Array.Empty<MapPoint>();

    [ObservableProperty]
    private double progress;

    [ObservableProperty]
    private string displayName = "";

    [ObservableProperty]
    private string mode = "";

    [ObservableProperty]
    private string purpose = "";

    [ObservableProperty]
    private string status = "";

    [ObservableProperty]
    private bool isApproximate;

    public DateTime LastUpdatedUtc { get; private set; }

    public void UpdateTarget(MovingEntityProjection projection)
    {
        PreviousPosition = CurrentPosition;
        CurrentPosition = projection.CurrentPosition;
        Destination = projection.Destination;
        RoutePolyline = projection.RoutePolyline;
        Progress = projection.Progress;
        DisplayName = projection.DisplayName;
        Mode = projection.Mode;
        Purpose = projection.Purpose;
        Status = projection.Status;
        IsApproximate = projection.IsApproximate;
        LastUpdatedUtc = DateTime.UtcNow;
    }

    public MapPoint Interpolate(DateTime utcNow)
    {
        var elapsed = utcNow - LastUpdatedUtc;
        var t = Math.Clamp(elapsed.TotalMilliseconds / AnimationDuration.TotalMilliseconds, 0.0, 1.0);
        return Interpolate(PreviousPosition, CurrentPosition, t);
    }

    public static MapPoint Interpolate(MapPoint previous, MapPoint current, double progress)
    {
        var t = Math.Clamp(progress, 0.0, 1.0);
        return new MapPoint(
            previous.X + (current.X - previous.X) * t,
            previous.Y + (current.Y - previous.Y) * t);
    }
}
