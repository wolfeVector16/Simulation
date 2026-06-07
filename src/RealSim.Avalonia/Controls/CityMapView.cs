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

        foreach (var primitive in viewModel.VisiblePrimitives.OrderBy(LayerOrder))
        {
            DrawPrimitive(context, viewModel, primitive, primitive == viewModel.SelectedPrimitive);
        }

        DrawMovingEntities(context, viewModel);
        DrawLabels(context, viewModel);
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
                context.DrawRectangle(fill, pen, new Rect(center.X - radius * 1.25, center.Y - radius * 0.65, radius * 2.5, radius * 1.3));
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

    private void DrawMovingEntities(DrawingContext context, MapViewModel viewModel)
    {
        var now = DateTime.UtcNow;
        foreach (var entity in viewModel.MovingEntities.Where(viewModel.IsMovingEntityVisible).OrderBy(entity => entity.Id))
        {
            var position = MapToScreen(viewModel, entity.Interpolate(now));
            var selected = entity == viewModel.SelectedMovingEntity;
            var fill = new SolidColorBrush(Color.Parse(selected ? "#FFF3B0" : MovingFill(entity.Kind)));
            var pen = new Pen(new SolidColorBrush(Color.Parse(selected ? "#FFD60A" : MovingStroke(entity.Kind))), selected ? 3.0 : 1.4);
            DrawSymbol(context, MovingSymbol(entity.Kind), position, MovingRadius(entity.Kind) * Math.Sqrt(viewModel.Zoom), fill, pen, selected);
        }
    }

    private void DrawLabels(DrawingContext context, MapViewModel viewModel)
    {
        if (!viewModel.ShowLabels)
        {
            return;
        }

        var labelLimit = viewModel.Zoom < 0.75 ? 26 : viewModel.Zoom < 1.5 ? 70 : 140;
        var labels = viewModel.VisiblePrimitives
            .Where(primitive => primitive.Name.Length > 0 && (viewModel.Zoom >= primitive.LabelMinZoom || primitive == viewModel.SelectedPrimitive))
            .OrderBy(primitive => primitive == viewModel.SelectedPrimitive ? -1 : primitive.LabelPriority)
            .ThenBy(primitive => primitive.Id)
            .Take(labelLimit)
            .ToArray();

        foreach (var primitive in labels)
        {
            DrawLabel(context, LabelPoint(viewModel, primitive), primitive.Name, primitive == viewModel.SelectedPrimitive);
        }

        foreach (var entity in viewModel.MovingEntities.Where(entity => entity == viewModel.SelectedMovingEntity))
        {
            DrawLabel(context, MapToScreen(viewModel, entity.Interpolate(DateTime.UtcNow)) + new Point(10, -24), entity.DisplayName, true);
        }
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
