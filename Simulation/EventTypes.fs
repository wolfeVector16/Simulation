namespace Simulation.Domain

open System

type DomainEvent =
    | PersonMoved of EventId * SimId * fromPlace: PlaceId * toPlace: PlaceId
    | JobStarted of EventId * SimId * PlaceId
    | JobLost of EventId * SimId * InstitutionId option
    | RentIncreased of EventId * HouseholdId * oldRent: decimal * newRent: decimal
    | BillDue of EventId * HouseholdId * amount: decimal
    | BillPaid of EventId * HouseholdId * amount: decimal
    | BillMissed of EventId * HouseholdId * amount: decimal
    | EvictionFiled of EventId * HouseholdId * UnitId
    | EvictionCompleted of EventId * HouseholdId * UnitId
    | IllnessOccurred of EventId * SimId
    | RelationshipChanged of EventId * RelationshipId * RelationshipDimensions
    | ConflictOccurred of EventId * actor: SimId * target: SimId * reason: string
    | ChildMissedSchool of EventId * SimId * InstitutionId option
    | SchoolDayCompleted of EventId * SimId * InstitutionId option
    | CrimeOccurred of EventId * NeighborhoodId * description: string
    | PoliceInteractionOccurred of EventId * SimId * InstitutionId
    | HospitalVisitOccurred of EventId * SimId * InstitutionId
    | BusinessOpened of EventId * PlaceId
    | BusinessClosed of EventId * PlaceId
    | PolicyPassed of EventId * policy: string
    | ServiceCapacityChanged of EventId * InstitutionId * oldCapacity: int * newCapacity: int
    | NeighborhoodReputationChanged of EventId * NeighborhoodId * delta: float
    | HouseholdBudgetChanged of EventId * HouseholdId * delta: decimal
    | TransportEventOccurred of EventId * TransportEvent
    | ActorMoved of EventId * ActorId * fromLocation: ActorLocation * toLocation: ActorLocation
    | ActorEnteredVehicle of EventId * ActorId * VehicleId
    | ActorExitedVehicle of EventId * ActorId * VehicleId * ActorLocation
    | ActorGainedVehicleControl of EventId * ActorId * VehicleId
    | ActorLostVehicleControl of EventId * ActorId * VehicleId
    | VehicleMoved of EventId * VehicleId * fromLocation: ActorLocation * toLocation: ActorLocation
    | VehicleCollisionOccurred of EventId * VehicleId * RoadSegmentId option
    | ActorEnteredBuilding of EventId * ActorId * BuildingId
    | ActorExitedBuilding of EventId * ActorId * BuildingId * ActorLocation
    | UnauthorizedEntryAttempted of EventId * ActorId * BuildingId
    | UnauthorizedEntrySucceeded of EventId * ActorId * BuildingId
    | UnauthorizedEntryFailed of EventId * ActorId * BuildingId
    | UnauthorizedVehicleAccessAttempted of EventId * ActorId * VehicleId
    | UnauthorizedVehicleAccessSucceeded of EventId * ActorId * VehicleId
    | UnauthorizedVehicleAccessFailed of EventId * ActorId * VehicleId
    | VehicleDamaged of EventId * VehicleId * severity: float
    | VehicleAlarmTriggered of EventId * VehicleId
    | ItemPurchased of EventId * ActorId * PlaceId * StreetItem * price: decimal
    | ItemTakenWithoutPayment of EventId * ActorId * PlaceId * StreetItem
    | ObjectUsed of EventId * ActorId * ItemId
    | PersonInteractionOccurred of EventId * ActorId * ActorId
    | ConflictStarted of EventId * ActorId * ActorId
    | ConflictEscalated of EventId * ActorId * ActorId
    | ConflictResolved of EventId * ActorId * ActorId
    | PropertyDamaged of EventId * ActorId * EntityRef * severity: float
    | TheftReported of EventId * ActorId option * PlaceId option
    | TrespassReported of EventId * ActorId option * BuildingId option
    | CrimeReported of EventId * ActorId option * PlaceId option * description: string
    | WitnessObservedEvent of EventId * witness: ActorId * subject: ActorId option * awareness: AwarenessLevel * description: string
    | PoliceDispatched of EventId * PoliceDispatch
    | PoliceArrived of EventId * PoliceDispatchId
    | EmergencyServiceCalled of EventId * ActorId option * PlaceId option
    | EmergencyServiceArrived of EventId * PlaceId option
    | ActorDetained of EventId * ActorId * InstitutionId option
    | ActorFined of EventId * ActorId * amount: decimal
    | ActorReleased of EventId * ActorId
    | ActorInjured of EventId * ActorId
    | BusinessInterrupted of EventId * PlaceId * severity: float
    | NeighborhoodSafetyChanged of EventId * NeighborhoodId * delta: float
    | InstitutionalTrustChanged of EventId * InstitutionId option * NeighborhoodId option * delta: float
    | ReputationChanged of EventId * ActorId * delta: float
    | WantedLevelChanged of EventId * ActorId * fromLevel: HeatLevel * toLevel: HeatLevel
    | HeatIncreased of EventId * ActorId * fromLevel: HeatLevel * toLevel: HeatLevel
    | HeatDecreased of EventId * ActorId * fromLevel: HeatLevel * toLevel: HeatLevel
    | RoadBuilt of EventId * CommandSource * RoadSegment * Lane list * cost: decimal
    | RoadModified of EventId * CommandSource * RoadSegmentId * RoadModification
    | RoadDestroyed of EventId * CommandSource * RoadSegmentId * RoadDestructionReason
    | RoadDamaged of EventId * CommandSource * RoadSegmentId * severity: float
    | RoadClosed of EventId * CommandSource * RoadSegmentId * reason: string
    | RoadReopened of EventId * CommandSource * RoadSegmentId
    | LaneConfigurationChanged of EventId * CommandSource * RoadSegmentId * Lane list
    | IntersectionModified of EventId * CommandSource * RoadNodeId
    | SignalTimingChanged of EventId * CommandSource * RoadNodeId
    | TransitRouteCreated of EventId * CommandSource * TransitRoute
    | TransitRouteModified of EventId * CommandSource * TransitRouteId
    | TransitRouteRemoved of EventId * CommandSource * TransitRouteId
    | UtilityBuilt of EventId * CommandSource * UtilitySource * cost: decimal
    | UtilityDamaged of EventId * CommandSource * utilityName: string * severity: float
    | UtilityDisabled of EventId * CommandSource * utilityName: string
    | UtilityRestored of EventId * CommandSource * utilityName: string
    | ParcelZoned of EventId * CommandSource * ParcelId * ZoneType * Density
    | ParcelRezoned of EventId * CommandSource * ParcelId * fromZone: ZoneType * toZone: ZoneType
    | BuildingConstructed of EventId * CommandSource * BuildingId * ParcelId * Building * cost: decimal
    | BuildingModified of EventId * CommandSource * BuildingId * BuildingModification
    | BuildingDamaged of EventId * CommandSource * BuildingId * severity: float
    | BuildingDestroyed of EventId * CommandSource * BuildingId * ParcelId * DestroyBuildingReason * cost: decimal
    | BuildingCondemned of EventId * CommandSource * BuildingId * reason: string
    | BuildingRepaired of EventId * CommandSource * BuildingId * cost: decimal
    | BuildingAbandoned of EventId * CommandSource * BuildingId
    | HousingUnitsAdded of EventId * CommandSource * BuildingId * count: int
    | HousingUnitsRemoved of EventId * CommandSource * BuildingId * count: int
    | HouseholdsDisplaced of EventId * CommandSource * BuildingId option * HouseholdId list
    | JobsCreated of EventId * CommandSource * JobId list
    | JobsLost of EventId * CommandSource * JobId list
    | InstitutionOpened of EventId * CommandSource * Institution
    | InstitutionClosed of EventId * CommandSource * InstitutionId
    | InstitutionCapacityChanged of EventId * CommandSource * InstitutionId * oldCapacity: int * newCapacity: int
    | PolicyRepealed of EventId * CommandSource * CityPolicy
    | BudgetChanged of EventId * CommandSource * department: string * oldAmount: decimal * newAmount: decimal
    | TaxRateChanged of EventId * CommandSource * TaxRates
    | BondIssued of EventId * CommandSource * amount: decimal * interestRate: float * termMonths: int
    | DisasterStarted of EventId * DisasterId * description: string
    | DisasterEnded of EventId * DisasterId
    | EmergencyActionTaken of EventId * CommandSource * EmergencyActionKind * EntityRef
type ResolvedCommand =
    { Validated: ValidatedCommand
      ActualCost: decimal
      AffectedEntities: EntityRef list
      Events: DomainEvent list
      Warnings: string list }
