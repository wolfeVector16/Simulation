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
    | WarehouseLogisticsZone
    | UtilityZone
    | ParkOpenSpaceZone
    | TransitOrientedZone
    | SpecialDistrictZone
type Density =
    | LowDensity
    | MediumDensity
    | HighDensity
