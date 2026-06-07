namespace Simulation.Domain

open System

type SnapshotData =
    { Tick: TickId
      Time: SimTime
      SimIds: SimId array
      HouseholdIds: HouseholdId array
      NeighborhoodIds: NeighborhoodId array
      InstitutionIds: InstitutionId array
      ActiveTripIds: TransportTripId array
      LaneIds: LaneId array }
type WorldSnapshot = private WorldSnapshot of SnapshotData
module WorldSnapshot =
    let create data = WorldSnapshot data
    let value (WorldSnapshot data) = data
type Pressure =
    { Entity: EntityId
      Kind: string
      Magnitude: Score
      Reason: DecisionReason option }
type PressureBatch =
    { Tick: TickId
      Partition: PartitionId
      Pressures: Pressure array }
type IntentBatch =
    { Tick: TickId
      Partition: PartitionId
      Intents: Intent array }
type ResolvedIntent =
    { Intent: Intent
      ResolutionRank: int
      TieBreaker: Guid }
type ResolvedIntentBatch =
    { Tick: TickId
      Partition: PartitionId
      Resolved: ResolvedIntent array }
type EventBatch =
    { Tick: TickId
      Partition: PartitionId
      Events: DomainEvent array
      OrderingRule: string }
type ChangedState =
    { ChangedPeople: SimId array
      ChangedHouseholds: HouseholdId array
      ChangedNeighborhoods: NeighborhoodId array
      ChangedInstitutions: InstitutionId array
      ChangedTrips: TransportTripId array
      ChangedRoadSegments: RoadSegmentId array
      ChangedLanes: LaneId array
      ChangedRelationships: RelationshipId array }
type PhaseDiagnostic =
    { Phase: string
      ItemsProcessed: int
      EventsEmitted: int
      RouteCalculations: int
      CacheHits: int
      CacheMisses: int
      FullScanWarnings: string array }
type TickResult =
    { Tick: TickId
      Events: EventBatch
      Changes: ChangedState
      NarrativeSummaries: string array
      Diagnostics: PhaseDiagnostic array }
type TickInput =
    { Tick: TickId
      Commands: CityCommand list }
