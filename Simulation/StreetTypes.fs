namespace Simulation.Domain

open System

type SimulationScale =
    | MacroCityScale
    | NeighborhoodScale
    | StreetScale
    | InteriorScale
type TickScale =
    | CityTick
    | StreetTick
    | TrafficTick
type ActorControl =
    | PlayerControlled of PlayerId
    | AiControlled
    | BackgroundAggregate
type HeatLevel =
    | NoHeat
    | LowHeat
    | ModerateHeat
    | HighHeat
    | CitywideAlert
type ActorLegalStatus =
    | NoLegalConcern
    | UnderInvestigation
    | DetainedStatus
    | FinedStatus
type ActorHealthStatus =
    | Healthy
    | Stressed
    | Injured
    | Incapacitated
type ActorActivity =
    | ActorIdle
    | ActorMoving
    | ActorShopping
    | ActorDriving
    | ActorInsideBuildingActivity
    | ActorFleeing
    | ActorReporting
    | ActorConflictActivity
type ActorLocation =
    | ActorAtPlace of PlaceId
    | ActorInsideBuilding of BuildingId
    | ActorInsideVehicle of VehicleId
    | ActorOnRoadSegment of RoadSegmentId
    | ActorInTransit of TransportTripId
type AccessibleArea =
    | ExteriorArea of PlaceId
    | BuildingInterior of BuildingId
    | RoomArea of RoomId
    | VehicleInterior of VehicleId
type VehicleAccessState =
    | VehiclePublicAccess
    | VehicleOwnedBy of HouseholdId
    | VehicleAuthorizedUsers of Set<ActorId>
    | VehicleRestrictedAccess
    | VehicleLocked
    | VehicleDisabled
    | VehicleAbandoned
    | VehicleEmergencyUseOnly
type BuildingAccessState =
    | BuildingPublic
    | BuildingPrivateResidence
    | BuildingEmployeesOnly
    | BuildingResidentsOnly
    | BuildingRestrictedInstitution
    | BuildingClosed
    | BuildingCondemnedAccess
    | BuildingEmergencyAccessOnly
type ItemCategory =
    | PersonalItem
    | PurchasedGood
    | Tool
    | DocumentOrPermit
    | VehicleAccessToken
    | EmergencySupply
    | DebugTestItem
type StreetItem =
    { Id: ItemId
      Name: string
      Category: ItemCategory
      Good: GoodKind option
      Price: decimal
      OwnerLabel: string option }
type ActorMemory =
    { Description: string
      Tick: int
      Salience: float }
type Actor =
    { Id: ActorId
      PersonId: SimId option
      HouseholdId: HouseholdId option
      Name: string
      Location: ActorLocation
      CurrentActivity: ActorActivity
      Control: ActorControl
      Health: ActorHealthStatus
      LegalStatus: ActorLegalStatus
      Heat: HeatLevel
      Reputation: float
      Relationships: Map<ActorId, float>
      Memories: ActorMemory list
      Inventory: Map<ItemId, StreetItem>
      CurrentVehicle: VehicleId option
      ActiveTrip: TransportTripId option }
type StreetVehicle =
    { Id: VehicleId
      Name: string
      Location: ActorLocation
      Access: VehicleAccessState
      Controller: ActorId option
      Disabled: bool
      Damage: float
      CurrentTrip: TransportTripId option }
type StreetBuilding =
    { Id: BuildingId
      Place: PlaceId
      Name: string
      Access: BuildingAccessState
      Neighborhood: NeighborhoodId option
      IsOpen: bool
      Condition: float }
type AwarenessLevel =
    | Unaware
    | HeardSomething
    | SawSomething
    | IdentifiedActor
    | Reported
type WitnessReaction =
    | Ignore
    | Flee
    | ReportToPolice
    | CallEmergency
    | Intervene
    | RecordMemory
    | ShareWithGroup
type ActiveSimulationArea =
    { Center: ActorLocation
      RadiusMeters: float
      DetailLevel: SimulationScale }
type PoliceDispatch =
    { Id: PoliceDispatchId
      ReportedActor: ActorId option
      IncidentPlace: PlaceId option
      PoliceInstitution: InstitutionId option
      Route: RoadSegmentId list
      ExpectedResponseMinutes: int
      Priority: int
      SourceEvent: EventId option }
type StreetSimulationState =
    { Actors: Map<ActorId, Actor>
      Vehicles: Map<VehicleId, StreetVehicle>
      Buildings: Map<BuildingId, StreetBuilding>
      PlaceConnections: Map<PlaceId, Set<PlaceId>>
      ActiveAreas: ActiveSimulationArea list
      Dispatches: Map<PoliceDispatchId, PoliceDispatch>
      RecentEventIds: EventId list }
