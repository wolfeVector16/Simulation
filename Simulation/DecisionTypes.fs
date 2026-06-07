namespace Simulation.Domain

open System

type DecisionReason =
    | NeedPressure of NeedKind
    | FinancialPressure
    | HousingInstability
    | SocialObligation of SimId
    | CaregivingResponsibility of SimId
    | FearOfConsequence
    | LongTermAspiration of AspirationKind
    | HabitualBehavior
    | InstitutionalRequirement of InstitutionId
    | OpportunityAvailable
    | AvoidanceBehavior of MemoryId
    | RelationshipPressure of SimId
    | HealthConstraint
    | ScheduleConstraint
    | LackOfAccess of InstitutionKind
    | TrustOrDistrust of InstitutionId
    | LegalConstraint
    | CulturalNorm of SocialNorm
    | PeerInfluence of GroupId
    | NoCarAvailable
    | TransitUnavailable
    | TransitUnreliable
    | ParkingTooExpensive
    | ParkingUnavailable
    | UnsafeWalkingRoute
    | UnsafeBikeRoute
    | BadWeather
    | HeavyCongestion
    | HabitualRoute
    | FamiliarRoute
    | AvoidsHighway
    | AvoidsToll
    | NeedsTripChain
    | NeedsChildPickup
    | MobilityLimitation
    | DeadlinePressure
    | MissedConnectionRisk
    | PreviousBadTripMemory of MemoryId option
    | FuelCostPressure
    | VehicleMaintenanceIssue
    | RoadClosureKnown
    | TransitStrikeOrCancellation
    | EmergencyPriority
    | FreightRestriction
type AgentAction =
    | PayBillAction of charge: BillCharge
    | DelayBillAction of charge: BillCharge
    | GoToWorkAction of PlaceId
    | SkipWorkAction
    | AttendSchoolAction of PlaceId
    | MissSchoolAction
    | SeekHelpAction of InstitutionId
    | CallPersonAction of SimId
    | StartConflictAction of SimId
    | RequestRepairAction of UnitId
    | MoveHouseholdAction of UnitId
    | FileEvictionAction of HouseholdId
    | NoOpAction
type Decision =
    { Actor: SimId option
      Household: HouseholdId option
      ChosenAction: AgentAction
      RejectedAlternatives: AgentAction list
      Reasons: DecisionReason list
      ExpectedConsequences: string list
      Confidence: float
      Urgency: float
      TimeCostMinutes: int
      MoneyCost: decimal
      SocialCost: float
      Risk: float }
type Intent =
    { Id: Guid
      PartitionKey: string
      Decision: Decision }
