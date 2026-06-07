using System;
using System.Globalization;
using System.Linq;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Media;
using Avalonia.Threading;
using RealSim.Avalonia.Models;
using RealSim.Avalonia.ViewModels;

namespace RealSim.Avalonia.Controls;

public sealed class CityMapView : Control
{
    public static readonly StyledProperty<MapViewModel?> ViewModelProperty =
        AvaloniaProperty.Register<CityMapView, MapViewModel?>(nameof(ViewModel));

    private readonly DispatcherTimer _animationTimer;
    private Point? _lastPointerPosition;
    private Point? _pressPosition;
    private bool _isDragging;

    public CityMapView()
    {
        ClipToBounds = true;
        Focusable = true;
        _animationTimer = new DispatcherTimer { Interval = TimeSpan.FromMilliseconds(33) };
        _animationTimer.Tick += (_, _) => InvalidateVisual();
        _animationTimer.Start();
    }

    public MapViewModel? ViewModel
    {
        get => GetValue(ViewModelProperty);
        set => SetValue(ViewModelProperty, value);
    }

    protected override void OnPropertyChanged(AvaloniaPropertyChangedEventArgs change)
    {
        base.OnPropertyChanged(change);

        if (change.Property == ViewModelProperty)
        {
            if (change.OldValue is MapViewModel oldViewModel)
            {
                oldViewModel.PropertyChanged -= OnViewModelPropertyChanged;
            }

            if (change.NewValue is MapViewModel newViewModel)
            {
                newViewModel.PropertyChanged += OnViewModelPropertyChanged;
            }

            InvalidateVisual();
        }
    }

    public class ResolvedMarker
    {
        public string Id { get; set; } = "";
        public Point BaseScreenPoint { get; set; }
        public Point OffsetScreenPoint { get; set; }
        public double Radius { get; set; }
        public object Source { get; set; } = null!;
    }

    private class LabelCandidate
    {
        public string Id { get; set; } = "";
        public string Text { get; set; } = "";
        public Point AnchorPoint { get; set; }
        public int Priority { get; set; }
        public bool IsSelected { get; set; }
    }

    public static bool IntersectsPublic(Rect r1, Rect r2)
    {
        return Intersects(r1, r2);
    }

    public System.Collections.Generic.List<ResolvedMarker> ResolveMarkers(MapViewModel viewModel)
    {
        var resolved = new System.Collections.Generic.List<ResolvedMarker>();
        var placedPoints = new System.Collections.Generic.List<Point>();
        var now = DateTime.UtcNow;

        var pointPrimitives = viewModel.VisiblePrimitives
            .Where(p => p.Kind is MapPrimitiveKind.Place or MapPrimitiveKind.Institution or MapPrimitiveKind.Household or MapPrimitiveKind.Destination or MapPrimitiveKind.EventMarker)
            .Select(p => new {
                Id = p.Id,
                BasePoint = p.Points.Count == 1 || p.Radius > 0.0 ? MapToScreen(viewModel, p.Points[0]) : ScreenCenter(viewModel, p.Points),
                Radius = Math.Max(p.Radius, 5.0) * Math.Sqrt(viewModel.Zoom),
                Source = (object)p,
                IsSelected = p == viewModel.SelectedPrimitive
            });

        var moving = viewModel.MovingEntities
            .Where(viewModel.IsMovingEntityVisible)
            .Select(e => new {
                Id = e.Id,
                BasePoint = MapToScreen(viewModel, e.Interpolate(now)),
                Radius = MovingRadius(e.Kind) * Math.Sqrt(viewModel.Zoom),
                Source = (object)e,
                IsSelected = e == viewModel.SelectedMovingEntity
            });

        var allCandidates = pointPrimitives.Concat(moving)
            .OrderByDescending(x => x.IsSelected)
            .ThenBy(x => x.Id)
            .ToArray();

        foreach (var c in allCandidates)
        {
            Point offsetPoint = c.BasePoint;
            int overlapCount = 0;
            foreach (var pt in placedPoints)
            {
                double dx = pt.X - c.BasePoint.X;
                double dy = pt.Y - c.BasePoint.Y;
                if (Math.Sqrt(dx * dx + dy * dy) < 8.0)
                {
                    overlapCount++;
                }
            }

            if (overlapCount > 0 && !c.IsSelected)
            {
                var hash = c.Id.GetHashCode();
                double angle = overlapCount * (2.0 * Math.PI / 6.0) + (Math.Abs(hash) % 10) * 0.1;
                double radius = 10.0 + (overlapCount / 6) * 4.0;
                offsetPoint = new Point(c.BasePoint.X + Math.Cos(angle) * radius, c.BasePoint.Y + Math.Sin(angle) * radius);
            }

            placedPoints.Add(offsetPoint);
            resolved.Add(new ResolvedMarker {
                Id = c.Id,
                BaseScreenPoint = c.BasePoint,
                OffsetScreenPoint = offsetPoint,
                Radius = c.Radius,
                Source = c.Source
            });
        }

        return resolved;
    }

    public override void Render(DrawingContext context)
    {
        base.Render(context);
        context.FillRectangle(new SolidColorBrush(Color.Parse("#F4F7F8")), Bounds);

        var viewModel = ViewModel;
        if (viewModel is null)
        {
            return;
        }

        DrawGrid(context, viewModel);

        var resolvedMarkers = ResolveMarkers(viewModel);
        var markerLookup = resolvedMarkers.ToDictionary(m => m.Id);

        foreach (var primitive in viewModel.VisiblePrimitives.OrderBy(LayerOrder))
        {
            if (markerLookup.TryGetValue(primitive.Id, out var resolved))
            {
                var fill = new SolidColorBrush(Color.Parse(primitive == viewModel.SelectedPrimitive ? "#FFF3B0" : primitive.Fill));
                var stroke = new SolidColorBrush(Color.Parse(primitive == viewModel.SelectedPrimitive ? "#FFD60A" : primitive.Stroke));
                var pen = new Pen(stroke, primitive.Thickness * Math.Sqrt(viewModel.Zoom) + (primitive == viewModel.SelectedPrimitive ? 2.4 : 0.0));
                DrawSymbol(context, primitive.Symbol, resolved.OffsetScreenPoint, resolved.Radius, fill, pen, primitive == viewModel.SelectedPrimitive);
            }
            else
            {
                DrawPrimitive(context, viewModel, primitive, primitive == viewModel.SelectedPrimitive);
            }
        }

        DrawMovingEntities(context, viewModel, markerLookup);
        DrawLabels(context, viewModel, resolvedMarkers);
    }

    protected override void OnPointerPressed(PointerPressedEventArgs e)
    {
        base.OnPointerPressed(e);
        Focus();
        _lastPointerPosition = e.GetPosition(this);
        _pressPosition = _lastPointerPosition;
        _isDragging = true;
        e.Pointer.Capture(this);
    }

    protected override void OnPointerMoved(PointerEventArgs e)
    {
        base.OnPointerMoved(e);
        if (!_isDragging || _lastPointerPosition is null || ViewModel is null)
        {
            return;
        }

        var position = e.GetPosition(this);
        var delta = position - _lastPointerPosition.Value;
        if (Math.Abs(delta.X) > 0.0 || Math.Abs(delta.Y) > 0.0)
        {
            ViewModel.PanBy(delta.X, delta.Y);
            InvalidateVisual();
        }

        _lastPointerPosition = position;
    }

    protected override void OnPointerReleased(PointerReleasedEventArgs e)
    {
        base.OnPointerReleased(e);
        var releasePosition = e.GetPosition(this);
        e.Pointer.Capture(null);

        if (_pressPosition is not null && ViewModel is not null)
        {
            var moved = releasePosition - _pressPosition.Value;
            if (Math.Sqrt(moved.X * moved.X + moved.Y * moved.Y) < 4.0)
            {
                var resolvedMarkers = ResolveMarkers(ViewModel);
                var hitMarker = resolvedMarkers
                    .Select(m => new { Marker = m, Dist = Math.Sqrt(Math.Pow(releasePosition.X - m.OffsetScreenPoint.X, 2.0) + Math.Pow(releasePosition.Y - m.OffsetScreenPoint.Y, 2.0)) })
                    .Where(x => x.Dist <= Math.Max(x.Marker.Radius + 4.0, 10.0))
                    .OrderBy(x => x.Dist)
                    .FirstOrDefault();

                if (hitMarker is not null)
                {
                    if (hitMarker.Marker.Source is MovingEntityViewModel mem)
                    {
                        ViewModel.SelectMovingEntity(mem);
                    }
                    else if (hitMarker.Marker.Source is MapPrimitive p)
                    {
                        ViewModel.SelectedPrimitive = p;
                    }
                    InvalidateVisual();
                }
                else
                {
                    var world = ScreenToMap(ViewModel, releasePosition);
                    var tolerance = 16.0 / Math.Max(0.1, EffectiveScale(ViewModel));
                    var movingEntity = ViewModel.MovingEntityAt(world.X, world.Y, tolerance);
                    if (movingEntity is not null)
                    {
                        ViewModel.SelectMovingEntity(movingEntity);
                    }
                    else
                    {
                        ViewModel.SelectAt(world.X, world.Y, tolerance);
                    }
                    InvalidateVisual();
                }
            }
        }

        _pressPosition = null;
        _lastPointerPosition = null;
        _isDragging = false;
    }

    protected override void OnPointerWheelChanged(PointerWheelEventArgs e)
    {
        base.OnPointerWheelChanged(e);
        if (ViewModel is null)
        {
            return;
        }

        ViewModel.ZoomBy(e.Delta.Y > 0 ? 1.12 : 1.0 / 1.12);
        InvalidateVisual();
    }

    private void OnViewModelPropertyChanged(object? sender, System.ComponentModel.PropertyChangedEventArgs e)
    {
        InvalidateVisual();
    }

    private void DrawGrid(DrawingContext context, MapViewModel viewModel)
    {
        var pen = new Pen(new SolidColorBrush(Color.Parse("#DFE7EA")), 1.0);
        for (var x = 0; x < Bounds.Width; x += 96)
        {
            context.DrawLine(pen, new Point(x + viewModel.PanX % 96, 0), new Point(x + viewModel.PanX % 96, Bounds.Height));
        }

        for (var y = 0; y < Bounds.Height; y += 96)
        {
            context.DrawLine(pen, new Point(0, y + viewModel.PanY % 96), new Point(Bounds.Width, y + viewModel.PanY % 96));
        }
    }

    private void DrawPrimitive(DrawingContext context, MapViewModel viewModel, MapPrimitive primitive, bool selected)
    {
        if (primitive.Points.Count == 0)
        {
            return;
        }

        var fill = new SolidColorBrush(Color.Parse(selected ? "#FFF3B0" : primitive.Fill));
        var stroke = new SolidColorBrush(Color.Parse(selected ? "#FFD60A" : primitive.Stroke));
        var pen = new Pen(stroke, primitive.Thickness * Math.Sqrt(viewModel.Zoom) + (selected ? 2.4 : 0.0));

        if (primitive.Kind is MapPrimitiveKind.Road or MapPrimitiveKind.RoadStatus or MapPrimitiveKind.TransitRoute or MapPrimitiveKind.ActiveRoute)
        {
            DrawPolyline(context, viewModel, primitive.Points, pen, primitive.LineStyle);
            return;
        }

        if (primitive.Radius > 0.0 || primitive.Points.Count == 1)
        {
            DrawPointSymbol(context, viewModel, primitive, fill, pen, selected);
            return;
        }

        var geometry = new StreamGeometry();
        using (var geometryContext = geometry.Open())
        {
            var first = MapToScreen(viewModel, primitive.Points[0]);
            geometryContext.BeginFigure(first, true);
            foreach (var point in primitive.Points.Skip(1))
            {
                geometryContext.LineTo(MapToScreen(viewModel, point));
            }

            geometryContext.EndFigure(true);
        }

        context.DrawGeometry(fill, pen, geometry);
        DrawInteriorSymbol(context, viewModel, primitive, selected);
    }

    private void DrawPolyline(
        DrawingContext context,
        MapViewModel viewModel,
        System.Collections.Generic.IReadOnlyList<MapPoint> points,
        Pen pen,
        MapLineStyle lineStyle)
    {
        for (var i = 0; i < points.Count - 1; i++)
        {
            DrawStyledLine(context, pen, MapToScreen(viewModel, points[i]), MapToScreen(viewModel, points[i + 1]), lineStyle);
        }
    }

    private static void DrawStyledLine(DrawingContext context, Pen pen, Point start, Point end, MapLineStyle lineStyle)
    {
        if (lineStyle == MapLineStyle.Solid)
        {
            context.DrawLine(pen, start, end);
            return;
        }

        var dx = end.X - start.X;
        var dy = end.Y - start.Y;
        var length = Math.Sqrt(dx * dx + dy * dy);
        if (length < 0.1)
        {
            return;
        }

        var dash = lineStyle == MapLineStyle.Dotted ? 2.0 : 12.0;
        var gap = lineStyle == MapLineStyle.Dotted ? 6.0 : 8.0;
        var ux = dx / length;
        var uy = dy / length;

        for (var walked = 0.0; walked < length; walked += dash + gap)
        {
            var segmentEnd = Math.Min(length, walked + dash);
            context.DrawLine(
                pen,
                new Point(start.X + ux * walked, start.Y + uy * walked),
                new Point(start.X + ux * segmentEnd, start.Y + uy * segmentEnd));
        }
    }

    private void DrawPointSymbol(DrawingContext context, MapViewModel viewModel, MapPrimitive primitive, IBrush fill, Pen pen, bool selected)
    {
        var point = MapToScreen(viewModel, primitive.Points[0]);
        var radius = Math.Max(primitive.Radius, 5.0) * Math.Sqrt(viewModel.Zoom);
        DrawSymbol(context, primitive.Symbol, point, radius, fill, pen, selected);
    }

    private void DrawInteriorSymbol(DrawingContext context, MapViewModel viewModel, MapPrimitive primitive, bool selected)
    {
        if (primitive.Symbol is MapSymbol.Polygon or MapSymbol.Square or MapSymbol.House or MapSymbol.Storefront or MapSymbol.Warehouse)
        {
            return;
        }

        var center = ScreenCenter(viewModel, primitive.Points);
        var brush = new SolidColorBrush(Color.Parse(selected ? "#FFF3B0" : "#FFFFFFCC"));
        var pen = new Pen(new SolidColorBrush(Color.Parse(selected ? "#FFD60A" : primitive.Stroke)), 1.3);
        DrawSymbol(context, primitive.Symbol, center, 5.5 * Math.Sqrt(viewModel.Zoom), brush, pen, selected);
    }

    private static void DrawSymbol(DrawingContext context, MapSymbol symbol, Point center, double radius, IBrush fill, Pen pen, bool selected)
    {
        switch (symbol)
        {
            case MapSymbol.Cross:
                context.DrawEllipse(fill, pen, center, radius, radius);
                context.DrawLine(pen, new Point(center.X - radius * 0.55, center.Y), new Point(center.X + radius * 0.55, center.Y));
                context.DrawLine(pen, new Point(center.X, center.Y - radius * 0.55), new Point(center.X, center.Y + radius * 0.55));
                break;
            case MapSymbol.Shield:
                DrawPolygon(context, fill, pen, new[]
                {
                    new Point(center.X, center.Y - radius),
                    new Point(center.X + radius * 0.85, center.Y - radius * 0.35),
                    new Point(center.X + radius * 0.55, center.Y + radius * 0.8),
                    new Point(center.X, center.Y + radius),
                    new Point(center.X - radius * 0.55, center.Y + radius * 0.8),
                    new Point(center.X - radius * 0.85, center.Y - radius * 0.35)
                });
                break;
            case MapSymbol.Diamond:
            case MapSymbol.Destination:
            case MapSymbol.Bike:
                DrawPolygon(context, fill, pen, new[]
                {
                    new Point(center.X, center.Y - radius),
                    new Point(center.X + radius, center.Y),
                    new Point(center.X, center.Y + radius),
                    new Point(center.X - radius, center.Y)
                });
                break;
            case MapSymbol.House:
                DrawPolygon(context, fill, pen, new[]
                {
                    new Point(center.X - radius, center.Y + radius * 0.75),
                    new Point(center.X - radius, center.Y - radius * 0.25),
                    new Point(center.X, center.Y - radius),
                    new Point(center.X + radius, center.Y - radius * 0.25),
                    new Point(center.X + radius, center.Y + radius * 0.75)
                });
                break;
            case MapSymbol.Storefront:
                context.DrawRectangle(fill, pen, new Rect(center.X - radius, center.Y - radius * 0.7, radius * 2.0, radius * 1.5));
                context.DrawLine(pen, new Point(center.X - radius, center.Y - radius * 0.2), new Point(center.X + radius, center.Y - radius * 0.2));
                break;
            case MapSymbol.Warehouse:
            case MapSymbol.Civic:
            case MapSymbol.School:
            case MapSymbol.Utility:
                context.DrawRectangle(fill, pen, new Rect(center.X - radius, center.Y - radius, radius * 2.0, radius * 2.0));
                if (symbol == MapSymbol.School)
                {
                    context.DrawLine(pen, new Point(center.X - radius * 0.55, center.Y), new Point(center.X + radius * 0.55, center.Y));
                }
                break;
            case MapSymbol.Tree:
                context.DrawEllipse(fill, pen, center, radius, radius);
                context.DrawLine(pen, new Point(center.X, center.Y + radius * 0.65), new Point(center.X, center.Y + radius * 1.25));
                break;
            case MapSymbol.Vehicle:
            case MapSymbol.Bus:
            case MapSymbol.Truck:
            case MapSymbol.Emergency:
                context.DrawEllipse(fill, pen, center, radius * 1.25, radius * 1.25);
                if (symbol == MapSymbol.Emergency)
                {
                    context.DrawLine(pen, new Point(center.X - radius * 0.45, center.Y), new Point(center.X + radius * 0.45, center.Y));
                    context.DrawLine(pen, new Point(center.X, center.Y - radius * 0.45), new Point(center.X, center.Y + radius * 0.45));
                }
                break;
            case MapSymbol.Warning:
                DrawPolygon(context, fill, pen, new[]
                {
                    new Point(center.X, center.Y - radius),
                    new Point(center.X + radius, center.Y + radius * 0.85),
                    new Point(center.X - radius, center.Y + radius * 0.85)
                });
                break;
            case MapSymbol.Person:
            case MapSymbol.Circle:
            default:
                context.DrawEllipse(fill, pen, center, radius, radius);
                break;
        }

        if (selected)
        {
            context.DrawEllipse(null, new Pen(new SolidColorBrush(Color.Parse("#FFD60A")), 2.0), center, radius + 5.0, radius + 5.0);
        }
    }

    private static void DrawPolygon(DrawingContext context, IBrush fill, Pen pen, Point[] points)
    {
        var geometry = new StreamGeometry();
        using (var geometryContext = geometry.Open())
        {
            geometryContext.BeginFigure(points[0], true);
            foreach (var point in points.Skip(1))
            {
                geometryContext.LineTo(point);
            }

            geometryContext.EndFigure(true);
        }

        context.DrawGeometry(fill, pen, geometry);
    }

    private void DrawMovingEntities(DrawingContext context, MapViewModel viewModel, System.Collections.Generic.Dictionary<string, ResolvedMarker> markerLookup)
    {
        var now = DateTime.UtcNow;
        var visibleEntities = viewModel.MovingEntities.Where(viewModel.IsMovingEntityVisible).OrderBy(entity => entity.Id).ToArray();
        if (viewModel.ShowRoutes)
        {
            foreach (var entity in visibleEntities.Where(entity => entity.RoutePolyline.Count >= 2))
            {
                var selected = entity == viewModel.SelectedMovingEntity;
                var routePen = new Pen(
                    new SolidColorBrush(Color.Parse(selected ? "#FFD60AAA" : RouteTrailStroke(entity.Kind))),
                    selected ? 3.2 : 1.3);
                DrawPolyline(context, viewModel, entity.RoutePolyline, routePen, MapLineStyle.Solid);
            }
        }

        foreach (var entity in visibleEntities)
        {
            var selected = entity == viewModel.SelectedMovingEntity;
            var fill = new SolidColorBrush(Color.Parse(selected ? "#FFF3B0" : MovingFill(entity.Kind)));
            var pen = new Pen(new SolidColorBrush(Color.Parse(selected ? "#FFD60A" : MovingStroke(entity.Kind))), selected ? 3.0 : 1.4);

            Point position;
            double radius;
            if (markerLookup.TryGetValue(entity.Id, out var resolved))
            {
                position = resolved.OffsetScreenPoint;
                radius = resolved.Radius;
            }
            else
            {
                position = MapToScreen(viewModel, entity.Interpolate(now));
                radius = MovingRadius(entity.Kind) * Math.Sqrt(viewModel.Zoom);
            }

            DrawSymbol(context, MovingSymbol(entity.Kind), position, radius, fill, pen, selected);

            if (entity.IsDelayedOrBlocked)
            {
                var markerCenter = position + new Point(0, -radius - 8.0);
                DrawSymbol(
                    context,
                    MapSymbol.Warning,
                    markerCenter,
                    5.5 * Math.Sqrt(viewModel.Zoom),
                    new SolidColorBrush(Color.Parse("#FFD166")),
                    new Pen(new SolidColorBrush(Color.Parse("#2B1700")), 1.2),
                    false);
            }
        }
    }

    private void DrawLabels(DrawingContext context, MapViewModel viewModel, System.Collections.Generic.List<ResolvedMarker> resolvedMarkers)
    {
        if (!viewModel.ShowLabels)
        {
            return;
        }

        var now = DateTime.UtcNow;
        var acceptedLabelRects = new System.Collections.Generic.List<Rect>();

        var occupiedRects = new System.Collections.Generic.List<Rect>();
        foreach (var m in resolvedMarkers)
        {
            occupiedRects.Add(new Rect(m.OffsetScreenPoint.X - m.Radius, m.OffsetScreenPoint.Y - m.Radius, m.Radius * 2.0, m.Radius * 2.0));
        }

        var candidates = new System.Collections.Generic.List<LabelCandidate>();

        if (viewModel.SelectedPrimitive is not null && !string.IsNullOrEmpty(viewModel.SelectedPrimitive.Name))
        {
            candidates.Add(new LabelCandidate
            {
                Id = viewModel.SelectedPrimitive.Id,
                Text = viewModel.SelectedPrimitive.Name,
                AnchorPoint = LabelPoint(viewModel, viewModel.SelectedPrimitive),
                Priority = 1,
                IsSelected = true
            });
        }

        if (viewModel.SelectedMovingEntity is not null)
        {
            var m = resolvedMarkers.FirstOrDefault(rm => rm.Id == viewModel.SelectedMovingEntity.Id);
            Point anchor = m != null ? m.OffsetScreenPoint + new Point(10, -24) : MapToScreen(viewModel, viewModel.SelectedMovingEntity.Interpolate(now)) + new Point(10, -24);
            candidates.Add(new LabelCandidate
            {
                Id = viewModel.SelectedMovingEntity.Id,
                Text = viewModel.SelectedMovingEntity.DisplayName,
                AnchorPoint = anchor,
                Priority = 1,
                IsSelected = true
            });
        }

        foreach (var p in viewModel.VisiblePrimitives)
        {
            if (p == viewModel.SelectedPrimitive || string.IsNullOrEmpty(p.Name))
            {
                continue;
            }

            bool allowed = false;
            if (viewModel.Zoom < 0.75)
            {
                allowed = p.Kind == MapPrimitiveKind.Neighborhood;
            }
            else if (viewModel.Zoom < 1.5)
            {
                allowed = p.Kind == MapPrimitiveKind.Neighborhood ||
                          p.Kind == MapPrimitiveKind.Institution ||
                          (p.Kind == MapPrimitiveKind.Road && (p.Category == "Highway" || p.Category == "Freeway" || p.Category == "Arterial" || p.Category == "Collector" || p.Thickness >= 3.5));
            }
            else
            {
                allowed = true;
            }

            if (!allowed)
            {
                continue;
            }

            int priority = p.Kind switch
            {
                MapPrimitiveKind.Institution => 3,
                MapPrimitiveKind.Neighborhood => 4,
                MapPrimitiveKind.Road => 5,
                MapPrimitiveKind.Building or MapPrimitiveKind.Place => 6,
                MapPrimitiveKind.Household => 7,
                _ => 8
            };

            Point anchor = LabelPoint(viewModel, p);
            if (p.Kind is MapPrimitiveKind.Place or MapPrimitiveKind.Institution or MapPrimitiveKind.Household or MapPrimitiveKind.Destination or MapPrimitiveKind.EventMarker)
            {
                var rm = resolvedMarkers.FirstOrDefault(m => m.Id == p.Id);
                if (rm != null)
                {
                    anchor = rm.OffsetScreenPoint + new Point(6, -18);
                }
            }

            candidates.Add(new LabelCandidate
            {
                Id = p.Id,
                Text = p.Name,
                AnchorPoint = anchor,
                Priority = priority,
                IsSelected = false
            });
        }

        if (viewModel.Zoom >= 1.5)
        {
            foreach (var e in viewModel.MovingEntities.Where(viewModel.IsMovingEntityVisible))
            {
                if (viewModel.SelectedMovingEntity != null && e.Id == viewModel.SelectedMovingEntity.Id)
                {
                    continue;
                }

                var rm = resolvedMarkers.FirstOrDefault(m => m.Id == e.Id);
                Point anchor = rm != null ? rm.OffsetScreenPoint + new Point(10, -24) : MapToScreen(viewModel, e.Interpolate(now)) + new Point(10, -24);

                candidates.Add(new LabelCandidate
                {
                    Id = e.Id,
                    Text = e.DisplayName,
                    AnchorPoint = anchor,
                    Priority = 2,
                    IsSelected = false
                });
            }
        }

        var orderedCandidates = candidates
            .OrderBy(c => c.Priority)
            .ThenBy(c => c.Id)
            .ToArray();

        foreach (var cand in orderedCandidates)
        {
            var formatted = new FormattedText(
                cand.Text,
                CultureInfo.CurrentCulture,
                FlowDirection.LeftToRight,
                new Typeface("Inter"),
                cand.IsSelected ? 13.0 : 11.0,
                Brushes.Black);

            double width = formatted.Width + 6.0;
            double height = formatted.Height + 4.0;

            var offsets = new[]
            {
                new Point(0, 0),
                new Point(0, -height - 4),
                new Point(0, height + 4),
                new Point(width + 4, 0),
                new Point(-width - 4, 0)
            };

            bool placed = false;
            foreach (var offset in offsets)
            {
                var candidateRect = new Rect(cand.AnchorPoint.X - 3.0 + offset.X, cand.AnchorPoint.Y - 2.0 + offset.Y, width, height);

                if (cand.IsSelected)
                {
                    DrawLabel(context, cand.AnchorPoint + offset, cand.Text, true);
                    acceptedLabelRects.Add(candidateRect);
                    placed = true;
                    break;
                }

                bool collides = false;
                foreach (var r in acceptedLabelRects)
                {
                    if (Intersects(candidateRect, r))
                    {
                        collides = true;
                        break;
                    }
                }

                if (!collides)
                {
                    foreach (var r in occupiedRects)
                    {
                        if (Intersects(candidateRect, r))
                        {
                            collides = true;
                            break;
                        }
                    }
                }

                if (!collides)
                {
                    DrawLabel(context, cand.AnchorPoint + offset, cand.Text, false);
                    acceptedLabelRects.Add(candidateRect);
                    placed = true;
                    break;
                }
            }

            if (!placed && cand.IsSelected)
            {
                DrawLabel(context, cand.AnchorPoint, cand.Text, true);
            }
        }
    }

    private static bool Intersects(Rect r1, Rect r2)
    {
        return !(r2.Left > r1.Right || r2.Right < r1.Left || r2.Top > r1.Bottom || r2.Bottom < r1.Top);
    }

    private static void DrawLabel(DrawingContext context, Point point, string text, bool selected)
    {
        var brush = new SolidColorBrush(Color.Parse(selected ? "#101820" : "#22313A"));
        var background = new SolidColorBrush(Color.Parse(selected ? "#FFF3B0EE" : "#FFFFFFDD"));
        var formatted = new FormattedText(
            text,
            CultureInfo.CurrentCulture,
            FlowDirection.LeftToRight,
            new Typeface("Inter"),
            selected ? 13.0 : 11.0,
            brush);
        var rect = new Rect(point.X - 3.0, point.Y - 2.0, formatted.Width + 6.0, formatted.Height + 4.0);
        context.FillRectangle(background, rect);
        context.DrawText(formatted, new Point(point.X, point.Y));
    }

    private Point LabelPoint(MapViewModel viewModel, MapPrimitive primitive)
    {
        if (primitive.Kind is MapPrimitiveKind.Road or MapPrimitiveKind.TransitRoute or MapPrimitiveKind.ActiveRoute or MapPrimitiveKind.RoadStatus)
        {
            var midpoint = primitive.Points.Count > 1 ? primitive.Points[primitive.Points.Count / 2] : primitive.Points[0];
            return MapToScreen(viewModel, midpoint) + new Point(5, -18);
        }

        return ScreenCenter(viewModel, primitive.Points) + new Point(6, -18);
    }

    private static int LayerOrder(MapPrimitive primitive)
    {
        return primitive.Kind switch
        {
            MapPrimitiveKind.Geography => 0,
            MapPrimitiveKind.Neighborhood => 1,
            MapPrimitiveKind.Parcel => 2,
            MapPrimitiveKind.Building => 3,
            MapPrimitiveKind.Road => 4,
            MapPrimitiveKind.RoadStatus => 5,
            MapPrimitiveKind.TransitRoute => 6,
            MapPrimitiveKind.ActiveRoute => 7,
            MapPrimitiveKind.Place => 8,
            MapPrimitiveKind.Institution => 9,
            MapPrimitiveKind.Household => 10,
            MapPrimitiveKind.Destination => 11,
            MapPrimitiveKind.EventMarker => 12,
            _ => 13
        };
    }

    private static string MovingFill(MovingEntityKind kind)
    {
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

    private static string MovingStroke(MovingEntityKind kind)
    {
        return kind switch
        {
            MovingEntityKind.Pedestrian => "#111111",
            MovingEntityKind.Bike => "#004B3A",
            MovingEntityKind.Bus => "#3C096C",
            MovingEntityKind.EmergencyVehicle => "#7D102D",
            _ => "#073B4C"
        };
    }

    private static string RouteTrailStroke(MovingEntityKind kind)
    {
        return kind switch
        {
            MovingEntityKind.Pedestrian or MovingEntityKind.Bike => "#00A89688",
            MovingEntityKind.Bus => "#8338EC88",
            MovingEntityKind.FreightTruck => "#7F553988",
            MovingEntityKind.EmergencyVehicle => "#EF476F88",
            _ => "#118AB288"
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
            MovingEntityKind.Pedestrian => 4.8,
            MovingEntityKind.Bike => 5.2,
            _ => 6.0
        };
    }

    private double EffectiveScale(MapViewModel viewModel)
    {
        var baseScale = Math.Min(Math.Max(0.1, Bounds.Width - 24.0) / 1000.0, Math.Max(0.1, Bounds.Height - 24.0) / 700.0);
        return baseScale * viewModel.Zoom;
    }

    private Point MapToScreen(MapViewModel viewModel, MapPoint point)
    {
        var scale = EffectiveScale(viewModel);
        return new Point(12.0 + viewModel.PanX + point.X * scale, 12.0 + viewModel.PanY + point.Y * scale);
    }

    private MapPoint ScreenToMap(MapViewModel viewModel, Point point)
    {
        var scale = EffectiveScale(viewModel);
        return new MapPoint((point.X - 12.0 - viewModel.PanX) / scale, (point.Y - 12.0 - viewModel.PanY) / scale);
    }

    private Point ScreenCenter(MapViewModel viewModel, System.Collections.Generic.IReadOnlyList<MapPoint> points)
    {
        return new Point(
            points.Select(point => MapToScreen(viewModel, point).X).Average(),
            points.Select(point => MapToScreen(viewModel, point).Y).Average());
    }
}
