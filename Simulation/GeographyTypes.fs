namespace Simulation.Domain

open System

type Coordinates =
    { X: float
      Y: float }
type RoadAccess =
    | DirectRoadAccess of RoadNodeId
    | NearestRoadAccess of maxDistanceMeters: float
    | NoRoadAccess
type TerrainKind =
    | Flatland
    | RollingHills
    | RiverValley
    | CoastalPlain
    | MountainFoothills
type NaturalFeatureKind =
    | River
    | Lake
    | Coastline
    | Hill
    | Floodplain
    | Forest
    | Parkland
    | ResourceArea
    | Wetland
type NaturalFeature =
    { Name: string
      Kind: NaturalFeatureKind
      Center: Coordinates
      RadiusMeters: float
      BarrierStrength: float
      AmenityValue: float
      FloodRisk: float
      PollutionBuffer: float }
type Geography =
    { Terrain: TerrainKind
      Features: NaturalFeature list
      BuildableLandRatio: float
      WaterAccess: float
      FloodRisk: float
      NaturalBarrierStrength: float
      OpenSpaceRatio: float }
