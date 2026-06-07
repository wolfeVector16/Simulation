namespace Simulation.Domain

open System

type SystemCommandSource =
    | DisasterSystem
    | DecaySystem
    | EconomySystem
    | LandlordSystem
    | BusinessSystem
    | InstitutionSystem
    | TrafficSystem
    | PolicySystem
    | WorldGenerationSystem
    | CrimeSystem
    | PoliceDispatchSystem
    | WitnessSystem
type CommandSource =
    | PlayerCommand of PlayerId option
    | SimulationSystemCommand of SystemCommandSource
    | ScenarioScriptCommand of ScenarioId option
    | DisasterCommand of DisasterId
    | InstitutionCommand of InstitutionId
    | HouseholdCommand of HouseholdId
    | DeveloperCommand of InstitutionId
    | ActorCommand of ActorId
    | DebugCommand
type PlayerAuthorityMode =
    | CityManagerMode
    | MayorMode
    | PlannerMode
    | SandboxGodMode
    | ScenarioDirectorMode
    | ObserverMode
    | StreetLevelActorMode of ActorId
type EntityRef =
    | ParcelRef of ParcelId
    | BuildingRef of BuildingId
    | RoadSegmentRef of RoadSegmentId
    | RoadNodeRef of RoadNodeId
    | InstitutionRef of InstitutionId
    | HouseholdRef of HouseholdId
    | UnitRef of UnitId
    | PlaceEntityRef of PlaceId
    | ActorRef of ActorId
    | VehicleRef of VehicleId
    | ItemRef of ItemId
    | LaneRef of LaneId
    | TransitRouteRef of TransitRouteId
    | DistrictRef of DistrictId
    | UnknownEntityRef of string
type EmergencyActionKind =
    | DeployEmergencyService
    | EvacuateArea
    | CreateEmergencyShelter
    | CloseHazardArea
    | RestoreCriticalAccess
type AssetRef =
    | BuildingAsset of BuildingId
    | RoadAsset of RoadSegmentId
    | UtilityAsset of string
    | InstitutionAsset of InstitutionId
    | ParcelAsset of ParcelId
type BuildBuildingCommand =
    { Source: CommandSource
      TargetParcel: ParcelId
      BuildingUse: BuildingUse
      IntendedOwner: OwnershipType
      ConstructionCost: decimal
      EstimatedConstructionTime: int
      FundingSource: FundingSource
      RequiredZoning: ZoneType
      ExpectedCapacity: int
      OptionalInstitutionKind: InstitutionKind option
      ParkingSupply: int
      UtilityDemand: BuildingUtilityDemand
      AccessibilityProfile: BuildingAccessibilityProfile }
type DestroyBuildingCommand =
    { Source: CommandSource
      BuildingId: BuildingId
      Reason: DestroyBuildingReason
      DemolitionCost: decimal
      DisplacementPolicy: DisplacementPolicy option
      PreserveHistoricalMemory: bool
      DebrisCleanupRequired: bool }
type ModifyBuildingCommand =
    { Source: CommandSource
      BuildingId: BuildingId
      Modification: BuildingModification
      Cost: decimal
      ExpectedDuration: int }
type BuildRoadCommand =
    { Source: CommandSource
      RoadClass: RoadClass
      FromNode: RoadNodeId
      ToNode: RoadNodeId
      Lanes: RoadLaneSpec list
      Sidewalks: bool
      BikeFacilities: BikeFacility
      TransitPriority: bool
      SpeedLimit: float
      Cost: decimal
      ConstructionTime: int
      AffectedParcels: ParcelId list
      AffectedNeighborhoods: NeighborhoodId list }
type ModifyRoadCommand =
    { Source: CommandSource
      RoadSegmentId: RoadSegmentId
      Modification: RoadModification
      Cost: decimal
      ConstructionTime: int
      TrafficDisruption: float }
type DestroyRoadCommand =
    { Source: CommandSource
      RoadSegmentId: RoadSegmentId
      Reason: RoadDestructionReason
      CleanupCost: decimal
      RerouteRequired: bool }
type BuildInstitutionCommand =
    { Source: CommandSource
      Name: string
      Kind: InstitutionKind
      Place: PlaceId option
      Neighborhood: NeighborhoodId
      Capacity: int
      Cost: decimal
      FundingSource: FundingSource }
type ModifyInstitutionCommand =
    { Source: CommandSource
      InstitutionId: InstitutionId
      Modification: InstitutionModification
      Cost: decimal }
type CloseInstitutionCommand =
    { Source: CommandSource
      InstitutionId: InstitutionId
      Reason: string
      PreserveServicesUntil: SimTime option }
type ZoneParcelsCommand =
    { Source: CommandSource
      ParcelIds: ParcelId list
      ZoneType: ZoneType
      AllowedDensity: Density
      Constraints: ZoneConstraint list
      EffectiveDate: SimTime }
type RezoneParcelsCommand =
    { Source: CommandSource
      ParcelIds: ParcelId list
      FromZone: ZoneType
      ToZone: ZoneType
      PoliticalCost: float
      LegalRisk: float
      DisplacementRisk: float
      EffectiveDate: SimTime }
type DezoneParcelsCommand =
    { Source: CommandSource
      ParcelIds: ParcelId list
      EffectiveDate: SimTime }
type BuildTransitRouteCommand =
    { Source: CommandSource
      RouteId: TransitRouteId
      Mode: TravelMode
      Stops: TransitStopId list
      HeadwayMinutes: int
      Cost: decimal }
type ModifyTransitRouteCommand =
    { Source: CommandSource
      RouteId: TransitRouteId
      Stops: TransitStopId list option
      HeadwayMinutes: int option
      Cost: decimal }
type RemoveTransitRouteCommand =
    { Source: CommandSource
      RouteId: TransitRouteId
      Reason: string }
type BuildUtilityCommand =
    { Source: CommandSource
      Utility: UtilitySource
      Cost: decimal }
type ModifyUtilityCommand =
    { Source: CommandSource
      UtilityName: string
      Capacity: float option
      MonthlyCost: decimal option
      Cost: decimal }
type DestroyUtilityCommand =
    { Source: CommandSource
      UtilityName: string
      Reason: string
      CleanupCost: decimal }
type SetBudgetCommand =
    { Source: CommandSource
      Department: string
      MonthlyAmount: decimal }
type PassPolicyCommand =
    { Source: CommandSource
      Policy: CityPolicy
      MonthlyCost: decimal
      EffectiveDate: SimTime }
type RepealPolicyCommand =
    { Source: CommandSource
      Policy: CityPolicy
      EffectiveDate: SimTime }
type IssueBondCommand =
    { Source: CommandSource
      Amount: decimal
      InterestRate: float
      TermMonths: int }
type SetTaxRateCommand =
    { Source: CommandSource
      Residential: float option
      Commercial: float option
      Industrial: float option }
type EmergencyActionCommand =
    { Source: CommandSource
      Action: EmergencyActionKind
      Target: EntityRef
      Cost: decimal }
type RepairAssetCommand =
    { Source: CommandSource
      Asset: AssetRef
      Cost: decimal
      ExpectedDuration: int }
type CondemnAssetCommand =
    { Source: CommandSource
      Asset: AssetRef
      Reason: string }
type CreateDistrictCommand =
    { Source: CommandSource
      District: District }
type ModifyDistrictCommand =
    { Source: CommandSource
      DistrictId: DistrictId
      Name: string option
      TransitPriority: float option
      FreightPriority: float option }
type StreetCommandContext =
    { CommandSource: CommandSource
      CommandActor: ActorId
      CommandLocation: ActorLocation
      CommandTick: TickId
      IntendedAction: string
      StreetExpectedConsequences: string list }
type AttemptResolution =
    | ResolveByRiskModel
    | ForceSuccess
    | ForceFailure
type MoveActorCommand =
    { Context: StreetCommandContext
      Destination: PlaceId }
type InteractWithPersonCommand =
    { Context: StreetCommandContext
      Target: ActorId }
type InteractWithObjectCommand =
    { Context: StreetCommandContext
      Target: ItemId }
type EnterVehicleCommand =
    { Context: StreetCommandContext
      Vehicle: VehicleId }
type ExitVehicleCommand =
    { Context: StreetCommandContext }
type DriveVehicleCommand =
    { Context: StreetCommandContext
      Vehicle: VehicleId
      Destination: PlaceId }
type UnauthorizedVehicleAccessCommand =
    { Context: StreetCommandContext
      Vehicle: VehicleId
      Resolution: AttemptResolution }
type EnterBuildingCommand =
    { Context: StreetCommandContext
      Building: BuildingId }
type ExitBuildingCommand =
    { Context: StreetCommandContext
      Destination: PlaceId option }
type UnauthorizedEntryCommand =
    { Context: StreetCommandContext
      Building: BuildingId
      Resolution: AttemptResolution }
type PurchaseItemCommand =
    { Context: StreetCommandContext
      Seller: PlaceId
      Item: StreetItem }
type TakeItemCommand =
    { Context: StreetCommandContext
      FromPlace: PlaceId
      Item: StreetItem
      Resolution: AttemptResolution }
type UseItemCommand =
    { Context: StreetCommandContext
      Item: ItemId }
type StartConflictCommand =
    { Context: StreetCommandContext
      Target: ActorId }
type FleeSceneCommand =
    { Context: StreetCommandContext
      Destination: PlaceId option }
type ReportCrimeCommand =
    { Context: StreetCommandContext
      ReportedActor: ActorId option
      IncidentPlace: PlaceId option }
type CallEmergencyServiceCommand =
    { Context: StreetCommandContext
      IncidentPlace: PlaceId option }
type TrespassCommand =
    { Context: StreetCommandContext
      Building: BuildingId }
type DamagePropertyCommand =
    { Context: StreetCommandContext
      Target: EntityRef
      Severity: float }
type SurrenderToPoliceCommand =
    { Context: StreetCommandContext }
type DebugTeleportActorCommand =
    { Context: StreetCommandContext
      Destination: ActorLocation }
type StreetCommand =
    | MoveActor of MoveActorCommand
    | InteractWithPerson of InteractWithPersonCommand
    | InteractWithObject of InteractWithObjectCommand
    | EnterVehicle of EnterVehicleCommand
    | ExitVehicle of ExitVehicleCommand
    | DriveVehicle of DriveVehicleCommand
    | AttemptUnauthorizedVehicleAccess of UnauthorizedVehicleAccessCommand
    | EnterBuilding of EnterBuildingCommand
    | ExitBuilding of ExitBuildingCommand
    | AttemptUnauthorizedEntry of UnauthorizedEntryCommand
    | PurchaseItem of PurchaseItemCommand
    | AttemptTakeItemWithoutPayment of TakeItemCommand
    | UseItem of UseItemCommand
    | StartConflict of StartConflictCommand
    | FleeScene of FleeSceneCommand
    | ReportCrime of ReportCrimeCommand
    | CallEmergencyService of CallEmergencyServiceCommand
    | Trespass of TrespassCommand
    | DamageProperty of DamagePropertyCommand
    | SurrenderToPolice of SurrenderToPoliceCommand
    | DebugTeleportActor of DebugTeleportActorCommand
type CityCommand =
    | BuildRoad of BuildRoadCommand
    | ModifyRoad of ModifyRoadCommand
    | DestroyRoad of DestroyRoadCommand
    | BuildBuilding of BuildBuildingCommand
    | ModifyBuilding of ModifyBuildingCommand
    | DestroyBuilding of DestroyBuildingCommand
    | BuildInstitution of BuildInstitutionCommand
    | ModifyInstitution of ModifyInstitutionCommand
    | CloseInstitution of CloseInstitutionCommand
    | ZoneParcels of ZoneParcelsCommand
    | RezoneParcels of RezoneParcelsCommand
    | DezoneParcels of DezoneParcelsCommand
    | BuildTransitRoute of BuildTransitRouteCommand
    | ModifyTransitRoute of ModifyTransitRouteCommand
    | RemoveTransitRoute of RemoveTransitRouteCommand
    | BuildUtility of BuildUtilityCommand
    | ModifyUtility of ModifyUtilityCommand
    | DestroyUtility of DestroyUtilityCommand
    | SetBudget of SetBudgetCommand
    | PassPolicy of PassPolicyCommand
    | RepealPolicy of RepealPolicyCommand
    | IssueBond of IssueBondCommand
    | SetTaxRate of SetTaxRateCommand
    | EmergencyAction of EmergencyActionCommand
    | RepairAsset of RepairAssetCommand
    | CondemnAsset of CondemnAssetCommand
    | CreateDistrict of CreateDistrictCommand
    | ModifyDistrict of ModifyDistrictCommand
    | StreetCommand of StreetCommand
type CommandRejection =
    | EntityNotFound of EntityRef
    | InsufficientFunds of required: Money * available: Money
    | UnauthorizedSource of CommandSource
    | InvalidZoning of ParcelId * ZoneType
    | OccupiedBuildingRequiresDisplacementPlan of BuildingId
    | WouldDisconnectRoadNetwork of RoadSegmentId
    | InvalidLaneConfiguration of RoadSegmentId
    | InstitutionCapacityInvalid of InstitutionId
    | UtilityCapacityInsufficient
    | LegalRestriction
    | ScenarioRuleViolation of string
    | InvariantViolation of string
type UnauthorizedContext =
    { Reason: string
      AffectedOwner: EntityRef option }
type LegalRiskContext =
    { Rule: string
      Severity: float }
type ConflictRiskContext =
    { Target: EntityRef option
      Severity: float }
type ActionFeasibility =
    | Impossible of CommandRejection list
    | FeasibleAuthorized
    | FeasibleUnauthorized of UnauthorizedContext
    | FeasibleIllegal of LegalRiskContext
    | FeasibleHostile of ConflictRiskContext
    | FeasibleEmergencyAuthority of InstitutionId option
type CommandWarning =
    | UnauthorizedAction
    | IllegalAction
    | WitnessRisk
    | PoliceResponseRisk
    | PropertyDamageRisk
    | ReputationRisk
    | HeatIncreaseRisk
    | BusinessInterruptionRisk
    | HouseholdConsequenceRisk
type ValidatedCommand =
    { Command: CityCommand
      AuthorityMode: PlayerAuthorityMode
      Source: CommandSource
      Actor: ActorId option
      Feasibility: ActionFeasibility
      CommandWarnings: CommandWarning list
      Warnings: string list }
type CommandValidationResult =
    | Valid of ValidatedCommand
    | Invalid of CommandRejection list
type CommandPreview =
    { Validation: CommandValidationResult
      ExpectedCost: decimal
      ExpectedAffectedHouseholds: HouseholdId list
      ExpectedAffectedJobs: JobId list
      ExpectedTrafficDisruption: float
      ExpectedServiceImpact: float
      ExpectedLandValueEffect: float
      ExpectedDisplacementRisk: float
      Warnings: string list
      RequiredFollowUpActions: string list }
type HeatDto =
    { Level: string }
type ActorDto =
    { Id: ActorId
      Name: string
      Position: Coordinates option
      CurrentActivity: string
      RelationshipToPlayer: string option
      Status: string
      Heat: string option }
type VehicleDto =
    { Id: VehicleId
      Name: string
      Position: Coordinates option
      Access: string
      ControlledByPlayer: bool }
type PlaceDto =
    { Id: PlaceId
      Name: string
      Kind: string
      Position: Coordinates option }
type EventDto =
    { Id: EventId
      Kind: string
      Description: string }
type InteractionPromptDto =
    { Id: InteractionId
      Label: string
      Command: CityCommand
      IsEnabled: bool
      DisabledReason: string option
      Warnings: string list }
type StreetViewQuery =
    { Center: ActorLocation option
      RadiusMeters: float }
type StreetViewSnapshot =
    { Player: ActorDto
      NearbyActors: ActorDto list
      NearbyVehicles: VehicleDto list
      NearbyPlaces: PlaceDto list
      NearbyEvents: EventDto list
      AvailableInteractions: InteractionPromptDto list
      Heat: HeatDto
      Time: SimTime }
