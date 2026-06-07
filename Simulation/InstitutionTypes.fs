namespace Simulation.Domain

open System

type InstitutionKind =
    | SchoolInstitution
    | HospitalInstitution
    | PoliceInstitution
    | WelfareInstitution
    | CourtInstitution
    | BankInstitution
    | EmployerInstitution
    | LandlordInstitution
    | TransitInstitution
type AccessRule =
    | OpenAccess
    | ResidentsOnly of NeighborhoodId
    | ChildrenOnly
    | EmployeesOnly
    | IncomeBelow of decimal
    | RequiresAppointment
    | RequiresFee of decimal
type InstitutionFailureMode =
    | OvercrowdedClassrooms
    | LimitedCounselorTime
    | ShiftInstability
    | InjuryRisk
    | DelayedRepairs
    | RentHikes
    | EvictionFilings
    | BusBunching
    | MissedConnections
    | LimitedEveningService
    | Understaffing
    | FundingShortfall
    | ServiceBacklog
type Institution =
    { Id: InstitutionId
      Name: string
      Kind: InstitutionKind
      Place: PlaceId option
      Neighborhood: NeighborhoodId
      Capacity: int
      Funding: decimal
      Quality: float
      StaffLevel: float
      Trust: float
      EligibilityRules: AccessRule list
      Backlog: int
      ServiceTimeMinutes: int
      Cost: decimal
      Reputation: float
      FailureModes: InstitutionFailureMode list }
