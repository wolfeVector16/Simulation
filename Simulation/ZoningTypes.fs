namespace Simulation.Domain

open System

type ZoneType =
    | ResidentialZone
    | CommercialZone
    | IndustrialZone
    | AgriculturalZone
    | CivicZone
    | ParkZone
    | Unzoned
    | SingleFamilyResidentialZone
    | MultifamilyResidentialZone
    | MixedUseZone
    | NeighborhoodCommercialZone
    | ShoppingCenterZone
    | OfficeZone
    | SchoolZone
    | MedicalZone
    | LightIndustrialZone
    | FlexIndustrialZone
    | WarehouseLogisticsZone
    | MixedUseProductionZone
    | HeavyIndustrialZone
    | HazardousIndustrialZone
    | ExtractiveIndustrialZone
    | UtilityZone
    | WasteManagementZone
    | ParkOpenSpaceZone
    | TransitOrientedZone
    | SpecialDistrictZone
type Density =
    | LowDensity
    | MediumDensity
    | HighDensity
