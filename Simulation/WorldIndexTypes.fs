namespace Simulation.Domain

open System

type DerivedIndexes =
    { PersonIdsByHousehold: Map<HouseholdId, SimId list>
      PersonIdsByNeighborhood: Map<NeighborhoodId, SimId list>
      RelationshipIdsByPerson: Map<SimId, RelationshipId list>
      GroupIdsByPerson: Map<SimId, GroupId list>
      UnitIdsByNeighborhood: Map<NeighborhoodId, UnitId list>
      InstitutionIdsByNeighborhood: Map<NeighborhoodId, InstitutionId list>
      StudentIdsBySchool: Map<InstitutionId, SimId list> }
[<Struct>]
type PersonIndex = PersonIndex of int
[<Struct>]
type HouseholdIndex = HouseholdIndex of int
[<Struct>]
type LaneIndex = LaneIndex of int
[<Struct>]
type RelationshipIndexRange =
    { Start: int
      Length: int }
[<Struct>]
type LaneIndexRange =
    { Start: int
      Length: int }
[<Struct>]
type NeedRuntimeState =
    { Hunger: float
      Energy: float
      Social: float
      Comfort: float }
[<Struct>]
type LaneRuntimeState =
    { Lane: LaneId
      Segment: RoadSegmentId
      Density: float
      SpeedKph: float
      QueueLength: int
      Blocked: bool }
[<Struct>]
type CandidateScore =
    { Entity: EntityId
      Score: float
      TieBreaker: Guid }
[<Struct>]
type TripCost =
    { Minutes: int
      Money: decimal
      Reliability: float
      Stress: float }
[<Struct>]
type MovementProposal =
    { Vehicle: VehicleId
      FromLane: LaneId
      ToLane: LaneId
      Priority: int
      TieBreaker: Guid }
type RuntimeIndexes =
    { PersonIndexById: Map<SimId, PersonIndex>
      PersonIdsByIndex: SimId array
      HouseholdIndexById: Map<HouseholdId, HouseholdIndex>
      HouseholdIdsByIndex: HouseholdId array
      LaneIndexById: Map<LaneId, LaneIndex>
      LaneIdsByIndex: LaneId array
      NeedsByPersonIndex: NeedRuntimeState array
      RelationshipsByPersonIndex: RelationshipIndexRange array
      LanesByIndex: LaneRuntimeState array
      IntersectionIncomingLaneRanges: Map<RoadNodeId, LaneIndexRange>
      TripsByPartition: Map<PartitionId, TransportTripId array>
      RouteCache: Map<PlaceId * PlaceId * TravelMode * int, TransportRoute>
      TravelTimeCache: Map<PlaceId * PlaceId * TravelMode * int, TripCost>
      CacheVersion: int }
type SimulationPerformanceBudget =
    { MaxCandidateActionsPerPersonPerTick: int
      MaxSocialInteractionsConsideredPerPersonPerDay: int
      MaxMemoriesInspectedPerDecision: int
      MaxRouteAlternativesPerTrip: int
      MaxReroutesPerTrip: int
      MaxInstitutionsConsideredPerRequest: int
      MaxSearchRadiusMeters: float
      MaxEventSummarySizePerTick: int
      MaxActiveTripsPerPartitionBeforeAggregation: int }
type RuntimePerformanceDiagnostics =
    { PhaseDiagnostics: PhaseDiagnostic list
      AgentsProcessed: int
      TripsProcessed: int
      IntentsGenerated: int
      EventsEmitted: int
      RouteCalculations: int
      CacheHits: int
      CacheMisses: int
      FullScanWarnings: string list
      MemoryCompactions: int
      EventLogCompactions: int
      PartitionWorkloads: Map<PartitionId, int> }
