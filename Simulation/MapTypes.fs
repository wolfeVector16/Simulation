namespace Simulation.Domain

open System

type CityMap =
    { Places: Map<PlaceId, Place>
      RoadNodes: Map<RoadNodeId, RoadNode>
      RoadSegments: RoadSegment list
      MetersPerMapUnit: float }
