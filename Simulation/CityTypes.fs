namespace Simulation.Domain

open System

type UtilityKind =
    | PowerUtility
    | WaterUtility
    | SewageUtility
    | GarbageUtility
type UtilitySource =
    { Name: string
      Kind: UtilityKind
      Capacity: float
      Used: float
      MonthlyCost: decimal
      Pollution: float
      Position: Coordinates }
type ServiceKind =
    | PoliceService
    | FireService
    | HealthService
    | EducationService
    | ParkService
    | TransitService
    | WasteService
type ServiceFacility =
    { Name: string
      Kind: ServiceKind
      CoverageRadius: float
      Capacity: float
      Used: float
      MonthlyCost: decimal
      Effectiveness: float
      Position: Coordinates }
type TaxRates =
    { Residential: float
      Commercial: float
      Industrial: float }
type Budget =
    { Treasury: decimal
      MonthlyIncome: decimal
      MonthlyExpenses: decimal
      Taxes: TaxRates
      Debt: decimal
      InterestRate: float }
type Demand =
    { Residential: float
      Commercial: float
      Industrial: float }
type Policies =
    { RecyclingProgram: bool
      SmokeDetectors: bool
      CarpoolIncentives: bool
      CleanAirAct: bool
      EducationCampaign: bool }
type CityIndicators =
    { Population: int
      Jobs: int
      Unemployment: float
      AverageLandValue: float
      AverageDesirability: float
      Pollution: float
      Crime: float
      FireRisk: float
      Education: float
      Health: float
      Traffic: float }
type AdvisorSeverity =
    | Info
    | Warning
    | Critical
type AdvisorMessage =
    { Severity: AdvisorSeverity
      Department: string
      Message: string }
type CityState =
    { Name: string
      Parcels: Map<ParcelId, Parcel>
      Utilities: UtilitySource list
      Services: ServiceFacility list
      Budget: Budget
      Demand: Demand
      Policies: Policies
      Indicators: CityIndicators
      Advisors: AdvisorMessage list }
