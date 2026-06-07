namespace Simulation.Domain

open System

type SettlementType =
    | CentralCity
    | InnerSuburb
    | OuterSuburb
    | SmallTown
    | RuralVillage
    | IndustrialDistrict
    | EdgeCity
    | CollegeTown
    | LogisticsHub
type UrbanArchetype =
    | HistoricGrid
    | PostwarSuburb
    | CulDeSacSubdivision
    | StripMallArterial
    | DowntownCore
    | IndustrialWaterfront
    | TransitOrientedDistrict
    | RuralMainStreet
    | WarehouseDistrict
    | MixedUseCorridor
    | WealthyLowDensitySuburb
    | AgingApartmentCorridor
    | CompanyTown
    | GentrifyingInnerNeighborhood
type LandUse =
    | SingleFamilyResidential
    | MultifamilyResidential
    | MixedUse
    | NeighborhoodCommercial
    | ShoppingCenter
    | OfficeUse
    | DowntownCommercial
    | CivicAdministrative
    | SchoolUse
    | HospitalMedical
    | IndustrialUse
    | WarehouseLogistics
    | UtilityUse
    | ParkOpenSpace
    | Agricultural
    | Vacant
    | ParkingUse
    | TransitStationArea
type Settlement =
    { Id: SettlementId
      Name: string
      SettlementType: SettlementType
      Archetype: UrbanArchetype
      Center: Coordinates
      PopulationTarget: int
      EmploymentTarget: int
      RoadPattern: string
      BlockSizeMeters: float
      DefaultDensity: string
      LandUseMix: Map<LandUse, float>
      MedianIncome: decimal
      TransitViability: float
      Walkability: float
      ParkingDependence: float
      HistoricalGrowthPhase: string }
type District =
    { Id: DistrictId
      Settlement: SettlementId
      Name: string
      Archetype: UrbanArchetype
      Center: Coordinates
      Neighborhoods: Set<NeighborhoodId>
      DominantLandUses: Set<LandUse>
      RoadClasses: Set<RoadClass>
      TransitPriority: float
      FreightPriority: float
      ParkingSupplyBias: float
      BuildingAgeRange: int * int
      IncomeBandLabel: string
      GrowthPressure: float
      HistoricConstraint: string option }
type Block =
    { Id: BlockId
      District: DistrictId
      Name: string
      Parcels: Set<ParcelId>
      BoundaryCenter: Coordinates
      ApproxAreaSqMeters: float
      DominantUse: LandUse
      RoadFrontage: Set<RoadSegmentId>
      PedestrianConnectivity: float
      ParkingSupply: int
      Buildable: bool }
type GeneratedJob =
    { Id: JobId
      Employer: InstitutionId option
      Place: PlaceId
      Kind: string
      WagePerDay: decimal
      RequiredSkill: SkillKind option
      StartMinute: int
      EndMinute: int
      Stability: float
      CommuteSensitivity: float }
type WorldScenario =
    | StrugglingInnerRingSuburb
    | FastGrowingSunbeltSuburb
    | OldIndustrialRiverCity
    | CollegeTownScenario
    | RuralCountySeat
    | CustomScenario of string
type WorldGenerationStep =
    | GenerateGeography
    | GenerateSettlements
    | GenerateTransportCorridors
    | GenerateDistricts
    | GenerateRoadHierarchy
    | GenerateLandUse
    | GenerateBlocks
    | GenerateParcels
    | GenerateBuildings
    | GenerateInstitutions
    | GenerateHouseholds
    | GenerateSocialGraph
    | GenerateTransit
    | GenerateEconomy
    | ValidateWorld
type ValidationIssue =
    | IsolatedParcel of ParcelId
    | UnreachableSchool of HouseholdId
    | InvalidLaneMovement of LaneId
    | NoFreightAccess of InstitutionId
    | ImplausibleCommutePattern of HouseholdId
    | TransitRouteWithoutDemand of TransitRouteId
    | JobWithoutLaborPool of JobId
    | HousingIncomeMismatch of NeighborhoodId
    | EmergencyCoverageGap of NeighborhoodId
    | RoadHierarchyDisconnected
    | InstitutionWithoutCatchment of InstitutionId
    | ShoppingAreaWithoutCustomerAccess of PlaceId
    | ParkingMismatch of NeighborhoodId
type ValidationStatus =
    | Passed
    | Repaired
    | IntentionalConstraint
    | Failed
type ValidationFinding =
    { Issue: ValidationIssue
      Status: ValidationStatus
      Message: string
      RepairAction: string option }
type WorldGenerationReport =
    { Seed: int
      Scenario: WorldScenario
      Steps: WorldGenerationStep list
      Assumptions: string list
      GeneratedSummary: string list
      Findings: ValidationFinding list
      Repairs: string list
      IntentionalConstraints: string list }
type Region =
    { Name: string
      Scenario: WorldScenario
      Geography: Geography
      Settlements: Set<SettlementId>
      RegionalCorridors: Set<RoadSegmentId>
      TransitCorridors: Set<TransitRouteId>
      EconomicRole: string
      HistoricalNarrative: string }
