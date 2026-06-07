namespace Simulation.Domain

open System

type FundingSource =
    | CityTreasury
    | BondFunding
    | FederalGrant
    | PrivateCapital
    | InstitutionBudget of InstitutionId
    | HouseholdFunds of HouseholdId
    | NoCostFunding
    | DebugFunding
type BuildingUtilityDemand =
    { Power: float
      Water: float
      Sewage: float
      Garbage: float }
type BuildingAccessibilityProfile =
    { WalkAccess: float
      BikeAccess: float
      TransitAccess: float
      CarAccess: float
      FreightAccess: float
      EmergencyAccess: float }
type DisplacementPolicy =
    | NoDisplacementProtection
    | RelocateHouseholds
    | PayRelocationAssistance of decimal
    | TemporaryShelterProvided
    | DirectHouseholdRelocation of HouseholdId list
type BuildingModification =
    | ChangeUse of BuildingUse
    | AddUnits of int
    | RemoveUnits of int
    | Renovate
    | Repair
    | RetrofitForAccessibility
    | EnergyUpgrade
    | AddParking of int
    | RemoveParking of int
    | AddCommercialSpace of int
    | ConvertToMixedUse
    | Condemn
    | Reopen
    | CloseTemporarily
type DestroyBuildingReason =
    | PlayerDemolition
    | FireDamage
    | FloodDamage
    | EarthquakeDamage
    | StructuralFailure
    | Redevelopment
    | EminentDomain
    | Abandonment
    | Condemnation
    | WarOrCivilUnrest
    | DebugRemoval
type RoadLaneSpec =
    { LaneType: LaneType
      Direction: LaneDirection
      AllowedModes: Set<TravelMode>
      PermittedMovements: Set<Movement> }
type RoadModification =
    | AddLane of RoadLaneSpec
    | RemoveLane of LaneId
    | AddBusLane
    | RemoveBusLane
    | AddProtectedBikeLane
    | AddSidewalk
    | ChangeSpeedLimit of float
    | AddTurnLane
    | ChangeSignalTiming
    | AddTrafficSignal
    | RemoveTrafficSignal
    | AddRampMeter
    | CloseForConstruction
    | ReopenRoad
    | ConvertToOneWay
    | ConvertToTwoWay
    | Pedestrianize
    | AddRoadParking
    | RemoveRoadParking
type RoadDestructionReason =
    | PlayerRemoval
    | Construction
    | RoadFloodDamage
    | RoadEarthquakeDamage
    | Sinkhole
    | BridgeFailure
    | ProtestBlockade
    | CrashClosure
    | MaintenanceFailure
    | DebugRoadRemoval
type ZoneConstraint =
    | MaximumHeightMeters of float
    | MinimumParkingSpaces of int
    | MaximumParkingSpaces of int
    | AffordableHousingShare of float
    | IndustrialBufferMeters of float
    | HistoricReviewRequired
    | EnvironmentalReviewRequired
type InstitutionServiceProgram =
    | ClassroomProgram
    | EmergencyRoomProgram
    | ShelterBedsProgram
    | BusOperationsProgram
    | RentalAssistanceProgram
    | FireInspectionProgram
    | CommunityOutreachProgram
type InstitutionModification =
    | ExpandInstitutionCapacity of int
    | ReduceInstitutionCapacity of int
    | ChangeInstitutionFunding of decimal
    | ChangeEligibilityRules of AccessRule list
    | ChangeServiceArea of NeighborhoodId list
    | AddServiceProgram of InstitutionServiceProgram
    | RemoveServiceProgram of InstitutionServiceProgram
    | ChangeInstitutionQuality of float
    | ReopenInstitution
type CityPolicy =
    | RentStabilization
    | InclusionaryZoning
    | ParkingMinimums
    | ParkingMaximums
    | TransitPriorityPolicy
    | VisionZeroPolicy
    | SchoolFundingFormula
    | HousingVoucherProgram
    | EmergencyRentalAssistance
    | HomelessnessResponseProgram
    | CleanAirRegulation
    | IndustrialBufferRequirement
    | ClimateResiliencePlan
    | AntiDisplacementProgram
    | SmallBusinessGrantProgram
    | PublicHealthCampaign
    | PoliceReformPolicy
    | FireInspectionProgramPolicy
    | BuildingCodeUpgrade
    | RoadMaintenancePriority
    | BikeNetworkPlan
    | CongestionPricing
    | LandValueTax
    | VacancyTax
    | ShortTermRentalRegulation
