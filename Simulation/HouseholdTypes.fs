namespace Simulation.Domain

open System

type Household =
    { Id: HouseholdId
      Name: string
      Home: PlaceId
      Members: Set<SimId>
      Funds: decimal
      MonthlyIncome: decimal
      MonthlyExpenses: decimal
      RentMonthly: decimal option
      Debt: decimal
      Assets: decimal
      Benefits: decimal
      HousingStatus: HousingStatus
      CareObligations: Set<SimId>
      ChoresBacklog: float
      FoodSecurity: float
      TransportationAccess: float
      Stability: float
      ConflictLevel: float
      SharedMemories: MemoryId list
      SharedGoals: string list
      Objects: HouseholdObject list
      BillsDue: decimal
      Cleanliness: float
      LotValue: decimal }
