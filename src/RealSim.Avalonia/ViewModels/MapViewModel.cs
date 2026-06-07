using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Linq;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class MapViewModel : ObservableObject
{
    [ObservableProperty]
    private IReadOnlyList<MapPrimitive> primitives = Array.Empty<MapPrimitive>();

    [ObservableProperty]
    private double zoom = 1.0;

    [ObservableProperty]
    private double panX;

    [ObservableProperty]
    private double panY;

    [ObservableProperty]
    private MapPrimitive? selectedPrimitive;

    [ObservableProperty]
    private MovingEntityViewModel? selectedMovingEntity;

    [ObservableProperty]
    private MapDisplayMode displayMode = MapDisplayMode.Clarity;

    [ObservableProperty]
    private DateTime animationTimeUtc = DateTime.UtcNow;

    [ObservableProperty]
    private bool showRoads = true;

    [ObservableProperty]
    private bool showBuildings = true;

    [ObservableProperty]
    private bool showTransit;

    [ObservableProperty]
    private bool showSims = true;

    [ObservableProperty]
    private bool showPedestrians = true;

    [ObservableProperty]
    private bool showVehicles = true;

    [ObservableProperty]
    private bool showRoutes;

    [ObservableProperty]
    private bool showTraffic = true;

    [ObservableProperty]
    private bool showEvents;

    [ObservableProperty]
    private bool showLabels = true;

    [ObservableProperty]
    private bool showDebugLayers;

    [ObservableProperty]
    private bool isFollowingSelectedMovement;

    public ObservableCollection<MovingEntityViewModel> MovingEntities { get; } = new();

    public ObservableCollection<LegendItemViewModel> LegendItems { get; } = new();

    public event Action<MapPrimitive?>? SelectionChanged;
    public event Action<MovingEntityViewModel?>? MovingSelectionChanged;

    partial void OnSelectedPrimitiveChanged(MapPrimitive? value)
    {
        if (value is not null && SelectedMovingEntity is not null)
        {
            SelectedMovingEntity = null;
        }

        SelectionChanged?.Invoke(value);
    }

    partial void OnSelectedMovingEntityChanged(MovingEntityViewModel? value)
    {
        if (value is not null && SelectedPrimitive is not null)
        {
            SelectedPrimitive = null;
        }

        if (value is null)
        {
            IsFollowingSelectedMovement = false;
        }

        MovingSelectionChanged?.Invoke(value);
    }

    public void Update(MapProjectionResult projection)
    {
        Primitives = projection.Primitives;
        UpdateLegend(projection.LegendItems ?? Array.Empty<LegendItem>());
        UpdateMovingEntities(projection.MovingEntities ?? Array.Empty<MovingEntityProjection>());
        SelectedPrimitive = SelectedPrimitive is null
            ? null
            : Primitives.FirstOrDefault(primitive => primitive.Id == SelectedPrimitive.Id && primitive.Kind == SelectedPrimitive.Kind);
        SelectedMovingEntity = SelectedMovingEntity is null
            ? null
            : MovingEntities.FirstOrDefault(entity => entity.Id == SelectedMovingEntity.Id);

        if (IsFollowingSelectedMovement)
        {
            CenterOnSelectedMovement();
        }
    }

    public IEnumerable<MapPrimitive> VisiblePrimitives => Primitives.Where(IsVisible);

    public bool IsVisible(MapPrimitive primitive)
    {
        if (primitive == SelectedPrimitive)
        {
            return true;
        }

        if (DisplayMode == MapDisplayMode.DebugRawPrimitives)
        {
            return ShowDebugLayers || primitive.Kind != MapPrimitiveKind.Label;
        }

        if (primitive.VisualRole == VisualRole.Debug && primitive.Kind != MapPrimitiveKind.Place && !ShowDebugLayers)
        {
            return false;
        }

        if (DisplayMode == MapDisplayMode.Traffic)
        {
            return primitive.Kind switch
            {
                MapPrimitiveKind.Geography or MapPrimitiveKind.Neighborhood => true,
                MapPrimitiveKind.Road or MapPrimitiveKind.RoadStatus => ShowRoads,
                MapPrimitiveKind.TransitRoute => ShowTransit,
                MapPrimitiveKind.Building or MapPrimitiveKind.Parcel => ShowBuildings,
                MapPrimitiveKind.Institution => ShowBuildings,
                MapPrimitiveKind.EventMarker => ShowEvents,
                MapPrimitiveKind.ActiveRoute or MapPrimitiveKind.Destination => false,
                _ => ShowDebugLayers
            };
        }

        if (DisplayMode == MapDisplayMode.Zoning)
        {
            return primitive.Kind switch
            {
                MapPrimitiveKind.Geography or MapPrimitiveKind.Neighborhood => true,
                MapPrimitiveKind.Parcel or MapPrimitiveKind.Building or MapPrimitiveKind.Institution => ShowBuildings,
                MapPrimitiveKind.Road => ShowRoads,
                MapPrimitiveKind.TransitRoute => false,
                MapPrimitiveKind.EventMarker => ShowEvents && ShowDebugLayers,
                _ => ShowDebugLayers
            };
        }

        return primitive.Kind switch
        {
            MapPrimitiveKind.Road => ShowRoads,
            MapPrimitiveKind.RoadStatus => false,
            MapPrimitiveKind.Building or MapPrimitiveKind.Institution => ShowBuildings,
            MapPrimitiveKind.Parcel => false,
            MapPrimitiveKind.Place => primitive.Radius > 0.0 ||
                                      primitive.Points.Count == 1 ||
                                      primitive.VisualRole == VisualRole.Park ||
                                      (ShowBuildings && primitive.Category is "School" or "Daycare" or "Civic"),
            MapPrimitiveKind.Household => ShowDebugLayers,
            MapPrimitiveKind.TransitRoute => false,
            MapPrimitiveKind.ActiveRoute or MapPrimitiveKind.Destination => false,
            MapPrimitiveKind.MovingEntity => false,
            MapPrimitiveKind.EventMarker => ShowEvents && IsImportantEvent(primitive),
            MapPrimitiveKind.Neighborhood => true,
            MapPrimitiveKind.Geography => false,
            _ => true
        };
    }

    public bool IsMovingEntityVisible(MovingEntityViewModel entity)
    {
        if (entity == SelectedMovingEntity)
        {
            return true;
        }

        return entity.Kind switch
        {
            MovingEntityKind.Pedestrian or MovingEntityKind.Bike or MovingEntityKind.Sim => ShowPedestrians,
            _ => ShowVehicles
        };
    }

    public bool AreRoutesVisible => ShowRoutes || SelectedMovingEntity is not null;

    public void AdvanceAnimationClock(DateTime utcNow)
    {
        AnimationTimeUtc = utcNow;
    }

    public void ZoomBy(double factor)
    {
        Zoom = Math.Clamp(Zoom * factor, 0.35, 5.0);
    }

    public void PanBy(double deltaX, double deltaY)
    {
        PanX += deltaX;
        PanY += deltaY;
    }

    public void ResetView()
    {
        Zoom = 1.0;
        PanX = 0.0;
        PanY = 0.0;
        IsFollowingSelectedMovement = false;
    }

    [RelayCommand]
    public void UseClarityMode()
    {
        DisplayMode = MapDisplayMode.Clarity;
        ShowDebugLayers = false;
        ShowRoutes = false;
        ShowEvents = false;
    }

    [RelayCommand]
    public void UseTrafficMode()
    {
        DisplayMode = MapDisplayMode.Traffic;
        ShowTraffic = true;
        ShowVehicles = true;
        ShowTransit = true;
    }

    [RelayCommand]
    public void UseZoningMode()
    {
        DisplayMode = MapDisplayMode.Zoning;
        ShowBuildings = true;
        ShowTraffic = false;
    }

    [RelayCommand]
    public void UseDebugRawPrimitivesMode()
    {
        DisplayMode = MapDisplayMode.DebugRawPrimitives;
        ShowDebugLayers = true;
        ShowRoutes = true;
        ShowEvents = true;
    }

    public MapPrimitive? SelectAt(double x, double y, double tolerance)
    {
        var selected = VisiblePrimitives
            .Select(primitive => new { Primitive = primitive, Distance = DistanceToPrimitive(primitive, x, y) })
            .Where(item => item.Distance <= tolerance)
            .OrderBy(item => LayerPriority(item.Primitive.Kind))
            .ThenBy(item => item.Distance)
            .ThenBy(item => item.Primitive.Id)
            .FirstOrDefault()
            ?.Primitive;

        SelectedPrimitive = selected;
        return selected;
    }

    public void SelectMovingEntity(MovingEntityViewModel entity)
    {
        SelectedMovingEntity = entity;
    }

    [RelayCommand]
    public void FollowSelectedMovement()
    {
        if (SelectedMovingEntity is null)
        {
            return;
        }

        IsFollowingSelectedMovement = true;
        CenterOnSelectedMovement();
    }

    [RelayCommand]
    public void StopFollowing()
    {
        IsFollowingSelectedMovement = false;
    }

    public void CenterOnSelectedMovement()
    {
        if (SelectedMovingEntity is null)
        {
            return;
        }

        var position = SelectedMovingEntity.CurrentPosition;
        PanX = 500.0 - position.X * Zoom;
        PanY = 350.0 - position.Y * Zoom;
    }

    public MovingEntityViewModel? MovingEntityAt(double x, double y, double tolerance)
    {
        return MovingEntities
            .Where(IsMovingEntityVisible)
            .Select(entity => new { Entity = entity, Distance = Math.Sqrt(Math.Pow(entity.CurrentPosition.X - x, 2.0) + Math.Pow(entity.CurrentPosition.Y - y, 2.0)) })
            .Where(item => item.Distance <= tolerance)
            .OrderBy(item => item.Distance)
            .ThenBy(item => item.Entity.Id)
            .FirstOrDefault()
            ?.Entity;
    }

    private void UpdateLegend(IReadOnlyList<LegendItem> items)
    {
        LegendItems.Clear();
        foreach (var item in items)
        {
            LegendItems.Add(new LegendItemViewModel(item));
        }
    }

    private void UpdateMovingEntities(IReadOnlyList<MovingEntityProjection> projections)
    {
        var existing = MovingEntities.ToDictionary(entity => entity.Id);
        var seen = new HashSet<string>();

        foreach (var projection in projections.OrderBy(item => item.Id))
        {
            if (existing.TryGetValue(projection.Id, out var entity))
            {
                entity.UpdateTarget(projection);
            }
            else
            {
                MovingEntities.Add(new MovingEntityViewModel(projection));
            }

            seen.Add(projection.Id);
        }

        for (var i = MovingEntities.Count - 1; i >= 0; i--)
        {
            if (!seen.Contains(MovingEntities[i].Id))
            {
                MovingEntities.RemoveAt(i);
            }
        }
    }

    private static int LayerPriority(MapPrimitiveKind kind)
    {
        return kind switch
        {
            MapPrimitiveKind.EventMarker => 0,
            MapPrimitiveKind.Destination => 1,
            MapPrimitiveKind.Household => 0,
            MapPrimitiveKind.Institution => 1,
            MapPrimitiveKind.Place => 2,
            MapPrimitiveKind.Building => 3,
            MapPrimitiveKind.Parcel => 4,
            MapPrimitiveKind.TransitRoute => 5,
            MapPrimitiveKind.Road => 6,
            MapPrimitiveKind.Neighborhood => 7,
            _ => 8
        };
    }

    private static bool IsImportantEvent(MapPrimitive primitive)
    {
        return primitive.Category.Contains("Road", StringComparison.OrdinalIgnoreCase) ||
               primitive.Name.Contains("blocked", StringComparison.OrdinalIgnoreCase) ||
               primitive.Name.Contains("crash", StringComparison.OrdinalIgnoreCase) ||
               primitive.Details.Contains("blocked", StringComparison.OrdinalIgnoreCase) ||
               primitive.Details.Contains("capacity", StringComparison.OrdinalIgnoreCase);
    }

    private static double DistanceToPrimitive(MapPrimitive primitive, double x, double y)
    {
        if (primitive.Points.Count == 0)
        {
            return double.PositiveInfinity;
        }

        if (primitive.Points.Count == 1 || primitive.Radius > 0.0)
        {
            var point = primitive.Points[0];
            return Math.Sqrt(Math.Pow(point.X - x, 2.0) + Math.Pow(point.Y - y, 2.0));
        }

        var best = double.PositiveInfinity;
        for (var i = 0; i < primitive.Points.Count - 1; i++)
        {
            best = Math.Min(best, DistanceToSegment(primitive.Points[i], primitive.Points[i + 1], x, y));
        }

        if (primitive.Kind is MapPrimitiveKind.Parcel or MapPrimitiveKind.Building or MapPrimitiveKind.Neighborhood)
        {
            best = Math.Min(best, DistanceToSegment(primitive.Points[^1], primitive.Points[0], x, y));
        }

        return best;
    }

    private static double DistanceToSegment(MapPoint a, MapPoint b, double x, double y)
    {
        var dx = b.X - a.X;
        var dy = b.Y - a.Y;
        if (Math.Abs(dx) < 0.0001 && Math.Abs(dy) < 0.0001)
        {
            return Math.Sqrt(Math.Pow(a.X - x, 2.0) + Math.Pow(a.Y - y, 2.0));
        }

        var t = Math.Clamp(((x - a.X) * dx + (y - a.Y) * dy) / (dx * dx + dy * dy), 0.0, 1.0);
        var px = a.X + t * dx;
        var py = a.Y + t * dy;
        return Math.Sqrt(Math.Pow(px - x, 2.0) + Math.Pow(py - y, 2.0));
    }
}
