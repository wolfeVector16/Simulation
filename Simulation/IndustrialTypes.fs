namespace Simulation.Domain

type PowerPlantKind =
    | Coal
    | Gas
    | Nuclear
    | Solar
    | Wind
    | Hydro
    | BatteryStorage

type IndustrialUse =
    | Warehouse
    | DistributionCenter
    | LastMileLogistics
    | LightManufacturing
    | Workshop
    | MakerSpace
    | EquipmentYard
    | ContractorYard
    | StorageYard
    | AutoRepair
    | FoodProduction
    | CleanManufacturing
    | ResearchAndDevelopmentFlex
    | HeavyManufacturing
    | ChemicalPlant
    | Refinery
    | SteelMill
    | Sawmill
    | GrainElevator
    | Mining
    | Quarry
    | CoalMine
    | Landfill
    | RecyclingCenter
    | WasteTransferStation
    | PowerPlant of PowerPlantKind

type ExternalityProfile =
    { AirPollution: float
      GroundPollution: float
      WaterPollution: float
      Noise: float
      Odor: float
      TruckTraffic: float
      FireRisk: float
      ExplosionRisk: float
      HazardousMaterialsRisk: float
      VisualBlight: float
      OperatingHoursIntensity: float }

type IndustrialBuildingForm =
    | SmallWorkshopBuilding
    | FlexIndustrialBuilding
    | WarehouseBox
    | LoadingDockWarehouse
    | StorageYardWithSheds
    | ContractorYardWithEquipment
    | LightManufacturingBuilding
    | CleanRoomManufacturing
    | FoodProductionFacility
    | HeavyFactory
    | TankFarm
    | GrainElevatorStructure
    | MineHead
    | QuarryPit
    | LandfillCell
    | WasteTransferBuilding
    | PowerGenerationFacility

type IndustrialJobProfile =
    { JobsPerHectare: float
      AverageWagePerDay: decimal
      RequiredSkill: SkillKind option
      SkillIntensity: float
      ShiftWorkIntensity: float
      InjuryRisk: float }

type IndustrialFreightDemand =
    { InboundTruckTripsPerDay: float
      OutboundTruckTripsPerDay: float
      VanTripsPerDay: float
      RailOrWaterAccessPreferred: bool
      LoadingSpaceNeed: float
      CurbPressure: float }

type IndustrialSite =
    { Use: IndustrialUse
      Form: IndustrialBuildingForm
      Externalities: ExternalityProfile
      Jobs: IndustrialJobProfile
      Freight: IndustrialFreightDemand
      MinimumResidentialBufferMeters: float
      CompatibleZones: Set<ZoneType> }

type NearbyLandUse =
    | NearbyResidential
    | NearbyCommercial
    | NearbyOffice
    | NearbyMixedUse
    | NearbyCivic
    | NearbyPark
    | NearbyIndustrial

type IndustrialCompatibilityResult =
    { Allowed: bool
      Warnings: string list
      RequiredBufferMeters: float
      FreightAccessScore: float }
