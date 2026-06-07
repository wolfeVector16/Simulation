namespace Simulation.Domain

open System

type SimulationRiskArea =
    | PoliticsAndGovernance
    | LandOwnership
    | HousingAffordability
    | CapitalAndFinance
    | TransportBehavior
    | ServiceQuality
    | Psychology
    | RelationshipDepth
    | Inequality
    | MaintenanceAndDecay
    | TimeCompression
    | AgentConflict
type SimulationRisk =
    { Area: SimulationRiskArea
      Severity: AdvisorSeverity
      Score: float
      Message: string }
type SimulationDiagnostics =
    { OverallFragility: float
      Risks: SimulationRisk list }
