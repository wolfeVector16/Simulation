namespace Simulation.Domain

open System

type EntityId =
    | SimEntity of SimId
    | ActorEntity of ActorId
    | HouseholdEntity of HouseholdId
    | InstitutionEntity of InstitutionId
    | RoadSegmentEntity of RoadSegmentId
    | LaneEntity of LaneId
    | TripEntity of TransportTripId
    | NeighborhoodEntity of NeighborhoodId
type RngPurpose =
    | WorldGenerationRng
    | HouseholdDecisionRng
    | TransportDecisionRng
    | ConflictTieBreakRng
    | MemoryDecayRng
    | IncidentGenerationRng
    | StreetOutcomeRng
    | WitnessReactionRng
type RngKey =
    { Seed: SimulationSeed
      Tick: TickId
      Partition: PartitionId
      Entity: EntityId option
      Purpose: RngPurpose }
