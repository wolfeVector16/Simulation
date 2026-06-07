using System;
using System.Collections.Generic;
using System.Linq;
using Avalonia;
using RealSim.Avalonia.Models;
using RealSim.Avalonia.ViewModels;

namespace RealSim.Avalonia.Controls;

public static class MarkerLayoutEngine
{
    public static IReadOnlyList<ResolvedMarker> Resolve(
        MapViewModel viewModel,
        Func<MapPoint, Point> mapToScreen,
        Func<IReadOnlyList<MapPoint>, Point> screenCenter)
    {
        var candidates = BuildCandidates(viewModel, mapToScreen, screenCenter).ToArray();
        var clustered = ClusterDenseCandidates(candidates, viewModel.Zoom);
        var placed = new List<ResolvedMarker>();

        foreach (var candidate in clustered.OrderBy(candidate => candidate.PlacementPriority).ThenBy(candidate => candidate.Id))
        {
            var resolved = TryPlace(candidate, placed, viewModel.Zoom);
            if (resolved is not null)
            {
                placed.Add(resolved);
            }
        }

        return placed;
    }

    private static IEnumerable<MarkerCandidate> BuildCandidates(
        MapViewModel viewModel,
        Func<MapPoint, Point> mapToScreen,
        Func<IReadOnlyList<MapPoint>, Point> screenCenter)
    {
        foreach (var primitive in viewModel.VisiblePrimitives)
        {
            if (primitive.Kind is not (MapPrimitiveKind.Place or MapPrimitiveKind.Institution or MapPrimitiveKind.Household or MapPrimitiveKind.Destination or MapPrimitiveKind.EventMarker))
            {
                continue;
            }

            if (primitive.Kind == MapPrimitiveKind.Place && primitive.Points.Count != 1 && primitive.Radius <= 0.0)
            {
                continue;
            }

            var basePoint = primitive.Points.Count == 1 || primitive.Radius > 0.0
                ? mapToScreen(primitive.Points[0])
                : screenCenter(primitive.Points);

            yield return new MarkerCandidate(
                primitive.Id,
                basePoint,
                Math.Max(primitive.Radius, MarkerRadiusForPrimitive(primitive)) * Math.Sqrt(viewModel.Zoom),
                primitive,
                primitive == viewModel.SelectedPrimitive,
                PlacementPriority(primitive),
                ClutterBehavior(primitive),
                primitive.Kind);
        }

        foreach (var entity in viewModel.MovingEntities.Where(viewModel.IsMovingEntityVisible))
        {
            yield return new MarkerCandidate(
                entity.Id,
                mapToScreen(entity.Interpolate(viewModel.AnimationTimeUtc)),
                MovingRadius(entity.Kind) * Math.Sqrt(viewModel.Zoom),
                entity,
                entity == viewModel.SelectedMovingEntity,
                entity == viewModel.SelectedMovingEntity ? 0 : 15,
                MapClutterBehavior.Cluster,
                MapPrimitiveKind.MovingEntity);
        }
    }

    private static IEnumerable<MarkerCandidate> ClusterDenseCandidates(IReadOnlyList<MarkerCandidate> candidates, double zoom)
    {
        var clusterRadius = zoom < 0.75 ? 24.0 : zoom < 1.5 ? 17.0 : 10.0;
        var consumed = new HashSet<string>();

        foreach (var candidate in candidates.OrderBy(candidate => candidate.PlacementPriority).ThenBy(candidate => candidate.Id))
        {
            if (consumed.Contains(candidate.Id))
            {
                continue;
            }

            if (candidate.IsSelected || candidate.ClutterBehavior != MapClutterBehavior.Cluster)
            {
                consumed.Add(candidate.Id);
                yield return candidate;
                continue;
            }

            var group = candidates
                .Where(other => !consumed.Contains(other.Id) &&
                                !other.IsSelected &&
                                other.Kind == candidate.Kind &&
                                other.ClutterBehavior == MapClutterBehavior.Cluster &&
                                Distance(other.BasePoint, candidate.BasePoint) <= clusterRadius)
                .OrderBy(other => other.Id)
                .ToArray();

            foreach (var item in group)
            {
                consumed.Add(item.Id);
            }

            if (group.Length <= 2 || zoom >= 1.8)
            {
                foreach (var item in group)
                {
                    yield return item;
                }
                continue;
            }

            var center = new Point(group.Average(item => item.BasePoint.X), group.Average(item => item.BasePoint.Y));
            yield return candidate with
            {
                Id = $"cluster:{candidate.Kind}:{StableHash(string.Join("|", group.Select(item => item.Id))):X8}",
                BasePoint = center,
                Radius = Math.Clamp(7.0 + Math.Sqrt(group.Length) * 2.0, 10.0, 22.0),
                Source = group.Select(item => item.Source).ToArray(),
                IsCluster = true,
                Count = group.Length,
                PlacementPriority = candidate.Kind == MapPrimitiveKind.MovingEntity ? 12 : 35
            };
        }
    }

    private static ResolvedMarker? TryPlace(MarkerCandidate candidate, IReadOnlyList<ResolvedMarker> placed, double zoom)
    {
        foreach (var point in Spiral(candidate.BasePoint, candidate.Id, zoom))
        {
            var bounds = new Rect(point.X - candidate.Radius, point.Y - candidate.Radius, candidate.Radius * 2.0, candidate.Radius * 2.0);
            if (!candidate.IsSelected && placed.Any(marker => Intersects(bounds, marker.Bounds)))
            {
                continue;
            }

            return new ResolvedMarker
            {
                Id = candidate.Id,
                BaseScreenPoint = candidate.BasePoint,
                OffsetScreenPoint = point,
                Radius = candidate.Radius,
                Source = candidate.Source,
                IsCluster = candidate.IsCluster,
                Count = candidate.Count,
                ClusterKind = candidate.Kind
            };
        }

        if (candidate.IsSelected || candidate.PlacementPriority <= 20)
        {
            return new ResolvedMarker
            {
                Id = candidate.Id,
                BaseScreenPoint = candidate.BasePoint,
                OffsetScreenPoint = candidate.BasePoint,
                Radius = candidate.Radius,
                Source = candidate.Source,
                IsCluster = candidate.IsCluster,
                Count = candidate.Count,
                ClusterKind = candidate.Kind
            };
        }

        return null;
    }

    private static IEnumerable<Point> Spiral(Point center, string id, double zoom)
    {
        yield return center;

        var phase = (StableHash(id) % 360) * Math.PI / 180.0;
        var ring1 = 12.0 * Math.Sqrt(Math.Max(0.5, zoom));
        for (var i = 0; i < 8; i++)
        {
            var angle = phase + i * Math.PI * 2.0 / 8.0;
            yield return new Point(center.X + Math.Cos(angle) * ring1, center.Y + Math.Sin(angle) * ring1);
        }

        var ring2 = 22.0 * Math.Sqrt(Math.Max(0.5, zoom));
        for (var i = 0; i < 12; i++)
        {
            var angle = phase + i * Math.PI * 2.0 / 12.0;
            yield return new Point(center.X + Math.Cos(angle) * ring2, center.Y + Math.Sin(angle) * ring2);
        }
    }

    public static int StableHash(string text)
    {
        unchecked
        {
            var hash = (int)2166136261;
            foreach (var ch in text)
            {
                hash ^= ch;
                hash *= 16777619;
            }

            return hash & 0x7FFFFFFF;
        }
    }

    public static bool Intersects(Rect first, Rect second)
    {
        return !(second.Left > first.Right || second.Right < first.Left || second.Top > first.Bottom || second.Bottom < first.Top);
    }

    private static int PlacementPriority(MapPrimitive primitive)
    {
        if (primitive.Kind == MapPrimitiveKind.Institution)
        {
            return 20;
        }

        return primitive.Kind switch
        {
            MapPrimitiveKind.Destination => 5,
            MapPrimitiveKind.EventMarker => 25,
            MapPrimitiveKind.Place => 40,
            MapPrimitiveKind.Household => 80,
            _ => 60
        };
    }

    private static MapClutterBehavior ClutterBehavior(MapPrimitive primitive)
    {
        return primitive.Kind switch
        {
            MapPrimitiveKind.Household => MapClutterBehavior.Cluster,
            MapPrimitiveKind.EventMarker => MapClutterBehavior.HideWhenCrowded,
            _ => primitive.ClutterBehavior
        };
    }

    private static double MarkerRadiusForPrimitive(MapPrimitive primitive)
    {
        return primitive.Kind switch
        {
            MapPrimitiveKind.Institution => 7.0,
            MapPrimitiveKind.Household => 3.5,
            MapPrimitiveKind.EventMarker => 6.0,
            MapPrimitiveKind.Destination => 6.5,
            _ => 5.0
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

    private static double Distance(Point first, Point second)
    {
        var dx = first.X - second.X;
        var dy = first.Y - second.Y;
        return Math.Sqrt(dx * dx + dy * dy);
    }

    private sealed record MarkerCandidate(
        string Id,
        Point BasePoint,
        double Radius,
        object Source,
        bool IsSelected,
        int PlacementPriority,
        MapClutterBehavior ClutterBehavior,
        MapPrimitiveKind Kind)
    {
        public bool IsCluster { get; init; }
        public int Count { get; init; } = 1;
    }
}
