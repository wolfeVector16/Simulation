namespace Simulation.Domain

open System

type SimulationMeta =
    { Seed: int
      Tick: int
      EventLog: DomainEvent list
      Decisions: Decision list
      Indexes: DerivedIndexes }
type World =
    { Day: int
      MinuteOfDay: int
      Geography: Geography
      Region: Region
      Settlements: Map<SettlementId, Settlement>
      Districts: Map<DistrictId, District>
      Blocks: Map<BlockId, Block>
      GeneratedJobs: Map<JobId, GeneratedJob>
      Sims: Map<SimId, Sim>
      Households: Map<HouseholdId, Household>
      Relationships: Map<RelationshipId, RelationshipEdge>
      Groups: Map<GroupId, SocialGroup>
      Institutions: Map<InstitutionId, Institution>
      Neighborhoods: Map<NeighborhoodId, Neighborhood>
      HousingUnits: Map<UnitId, HousingUnit>
      Memories: Map<MemoryId, Memory>
      Street: StreetSimulationState
      Transport: TransportState
      Runtime: RuntimeIndexes
      Performance: SimulationPerformanceBudget
      PerformanceDiagnostics: RuntimePerformanceDiagnostics
      Map: CityMap
      City: CityState
      Diagnostics: SimulationDiagnostics
      GenerationReport: WorldGenerationReport
      ExternalLedger: ExternalSectorLedger
      Meta: SimulationMeta }
