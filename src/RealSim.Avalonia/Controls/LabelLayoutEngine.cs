using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using Avalonia;
using Avalonia.Media;
using RealSim.Avalonia.Models;
using RealSim.Avalonia.ViewModels;

namespace RealSim.Avalonia.Controls;

public sealed record PlacedLabel(string Id, string Text, Point Point, Rect Bounds, bool IsSelected);

public static class LabelLayoutEngine
{
    public static int LabelBudget(double zoom)
    {
        return zoom < 0.75 ? 5 : zoom < 1.5 ? 10 : 15;
    }

    public static IReadOnlyList<PlacedLabel> PlaceLabels(
        MapViewModel viewModel,
        IReadOnlyList<ResolvedMarker> resolvedMarkers,
        Func<MapPrimitive, Point> labelPoint,
        Func<MapPoint, Point> mapToScreen)
    {
        if (!viewModel.ShowLabels)
        {
            return Array.Empty<PlacedLabel>();
        }

        var occupiedRects = resolvedMarkers.Select(marker => marker.Bounds).ToList();
        var accepted = new List<PlacedLabel>();
        var candidates = BuildLabelCandidates(viewModel, resolvedMarkers, labelPoint, mapToScreen)
            .OrderBy(candidate => candidate.Priority)
            .ThenBy(candidate => candidate.Id)
            .ToArray();

        var budget = LabelBudget(viewModel.Zoom);
        foreach (var candidate in candidates)
        {
            if (!candidate.IsSelected && accepted.Count(label => !label.IsSelected) >= budget)
            {
                continue;
            }

            foreach (var offset in Offsets(candidate.Text, candidate.IsSelected))
            {
                var point = candidate.AnchorPoint + offset;
                var bounds = Measure(point, candidate.Text, candidate.IsSelected);
                var collides = !candidate.IsSelected &&
                               (accepted.Any(label => MarkerLayoutEngine.Intersects(bounds, label.Bounds)) ||
                                occupiedRects.Any(rect => MarkerLayoutEngine.Intersects(bounds, rect)));

                if (collides)
                {
                    continue;
                }

                accepted.Add(new PlacedLabel(candidate.Id, candidate.Text, point, bounds, candidate.IsSelected));
                break;
            }

            if (candidate.IsSelected && accepted.All(label => label.Id != candidate.Id))
            {
                var bounds = Measure(candidate.AnchorPoint, candidate.Text, true);
                accepted.Add(new PlacedLabel(candidate.Id, candidate.Text, candidate.AnchorPoint, bounds, true));
            }
        }

        return accepted;
    }

    private static IEnumerable<LabelCandidate> BuildLabelCandidates(
        MapViewModel viewModel,
        IReadOnlyList<ResolvedMarker> resolvedMarkers,
        Func<MapPrimitive, Point> labelPoint,
        Func<MapPoint, Point> mapToScreen)
    {
        if (viewModel.SelectedPrimitive is not null && !string.IsNullOrEmpty(viewModel.SelectedPrimitive.Name))
        {
            yield return new LabelCandidate(viewModel.SelectedPrimitive.Id, viewModel.SelectedPrimitive.Name, labelPoint(viewModel.SelectedPrimitive), 0, true);
        }

        if (viewModel.SelectedMovingEntity is not null)
        {
            var marker = resolvedMarkers.FirstOrDefault(marker => marker.Id == viewModel.SelectedMovingEntity.Id);
            var anchor = marker is not null
                ? marker.OffsetScreenPoint + new Point(10, -24)
                : mapToScreen(viewModel.SelectedMovingEntity.Interpolate(viewModel.AnimationTimeUtc)) + new Point(10, -24);
            yield return new LabelCandidate(viewModel.SelectedMovingEntity.Id, viewModel.SelectedMovingEntity.DisplayName, anchor, 0, true);
        }

        foreach (var primitive in viewModel.VisiblePrimitives)
        {
            if (primitive == viewModel.SelectedPrimitive || string.IsNullOrWhiteSpace(primitive.Name) || !AllowedAtZoom(primitive, viewModel.Zoom, viewModel.DisplayMode))
            {
                continue;
            }

            var marker = resolvedMarkers.FirstOrDefault(marker => marker.Id == primitive.Id);
            var anchor = marker is not null ? marker.OffsetScreenPoint + new Point(6, -18) : labelPoint(primitive);
            yield return new LabelCandidate(primitive.Id, primitive.Name, anchor, Priority(primitive), false);
        }
    }

    private static bool AllowedAtZoom(MapPrimitive primitive, double zoom, MapDisplayMode mode)
    {
        if (mode == MapDisplayMode.DebugRawPrimitives)
        {
            return zoom >= primitive.LabelMinZoom;
        }

        if (mode == MapDisplayMode.Clarity)
        {
            if (primitive.Kind is MapPrimitiveKind.ActiveRoute or MapPrimitiveKind.TransitRoute or MapPrimitiveKind.MovingEntity or MapPrimitiveKind.Building or MapPrimitiveKind.Place)
            {
                return false;
            }

            if (zoom < 0.75)
            {
                return primitive.Kind == MapPrimitiveKind.Neighborhood;
            }

            if (zoom < 1.5)
            {
                return primitive.Kind == MapPrimitiveKind.Institution;
            }

            return primitive.Kind == MapPrimitiveKind.Institution ||
                   (primitive.Kind == MapPrimitiveKind.Road && IsMajorRoad(primitive));
        }

        if (zoom < 0.75)
        {
            return primitive.Kind == MapPrimitiveKind.Neighborhood ||
                   (primitive.Kind == MapPrimitiveKind.Road && IsMajorRoad(primitive));
        }

        if (zoom < 1.5)
        {
            return primitive.Kind == MapPrimitiveKind.Neighborhood ||
                   primitive.Kind == MapPrimitiveKind.Institution ||
                   (primitive.Kind == MapPrimitiveKind.Road && IsMajorRoad(primitive));
        }

        return primitive.Kind is MapPrimitiveKind.Institution or MapPrimitiveKind.Neighborhood ||
               (primitive.Kind == MapPrimitiveKind.Road && IsMajorRoad(primitive)) ||
               primitive.Kind == MapPrimitiveKind.EventMarker ||
               (primitive.Kind is MapPrimitiveKind.Building or MapPrimitiveKind.Place && primitive.LabelPriority <= 55);
    }

    private static int Priority(MapPrimitive primitive)
    {
        return primitive.Kind switch
        {
            MapPrimitiveKind.Institution => 2,
            MapPrimitiveKind.Neighborhood => 3,
            MapPrimitiveKind.Road => 4,
            MapPrimitiveKind.EventMarker => 5,
            MapPrimitiveKind.Building or MapPrimitiveKind.Place => 7,
            _ => 9
        };
    }

    private static bool IsMajorRoad(MapPrimitive primitive)
    {
        return primitive.Category is "Highway" or "Freeway" or "Arterial" or "Collector" || primitive.Thickness >= 4.5;
    }

    private static IEnumerable<Point> Offsets(string text, bool selected)
    {
        var bounds = Measure(new Point(0, 0), text, selected);
        yield return new Point(0, 0);
        yield return new Point(0, -bounds.Height - 4);
        yield return new Point(bounds.Width + 4, 0);
        yield return new Point(0, bounds.Height + 4);
        yield return new Point(-bounds.Width - 4, 0);
    }

    private static Rect Measure(Point point, string text, bool selected)
    {
        var fontSize = selected ? 13.0 : 11.0;
        var width = Math.Max(12.0, text.Length * fontSize * 0.58) + 6.0;
        var height = fontSize * 1.35 + 4.0;
        return new Rect(point.X - 3.0, point.Y - 2.0, width, height);
    }

    private sealed record LabelCandidate(string Id, string Text, Point AnchorPoint, int Priority, bool IsSelected);
}
