namespace Simulation.Domain

open System

type OwnershipType =
    | PersonOwner of SimId
    | HouseholdOwner of HouseholdId
    | CorporateOwner
    | PublicOwner
    | CooperativeOwner
    | InstitutionOwner of InstitutionId
type HousingStatus =
    | OwnsHome
    | Rents
    | StayingWithFamily
    | Shelter
    | Unsheltered
type LegalHousingStatus =
    | LeaseActive
    | LeaseEnding
    | EvictionFiledStatus
    | Evicted
    | InformalArrangement
type HousingUnit =
    { Id: UnitId
      Lot: LotId
      Neighborhood: NeighborhoodId
      Owner: OwnershipType
      Occupants: Set<HouseholdId>
      RentMonthly: decimal option
      MortgageMonthly: decimal option
      Condition: float
      SoftCapacity: int
      HardCapacity: int
      UtilityAccess: Set<UtilityKind>
      LegalStatus: LegalHousingStatus
      Habitability: float
      EvictionRisk: float
      Vacancy: bool }
type Neighborhood =
    { Id: NeighborhoodId
      Name: string
      Residents: Set<HouseholdId>
      Lots: Set<LotId>
      Institutions: Set<InstitutionId>
      Businesses: Set<PlaceId>
      LandValue: float
      RentPressure: float
      Safety: float
      Pollution: float
      Walkability: float
      TransitAccess: float
      SocialCohesion: float
      Reputation: float
      VacancyRate: float
      SchoolAccess: float
      HealthAccess: float
      EmploymentAccess: float
      ServiceQuality: float
      InstitutionalTrust: float
      InformalSupportCapacity: float
      SharedMemories: MemoryId list }
