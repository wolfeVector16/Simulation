namespace Simulation.Domain

open System

type PlaceKind =
    | Residence
    | Workplace
    | Commercial
    | Industrial
    | Warehouse
    | OutsideConnection
    | School
    | Daycare
    | Park
    | Civic
type CommercialOffering =
    { Good: GoodKind
      Intent: PurchaseIntent
      Price: decimal
      Appeal: float
      TargetStock: float }
type ProductionRecipe =
    { Output: GoodKind
      UnitsPerDay: float
      Inputs: Map<GoodKind, float> }
type PlaceEconomy =
    { Inventory: Map<GoodKind, float>
      Sells: CommercialOffering list
      Produces: ProductionRecipe list
      ImportsPerDay: Map<GoodKind, float>
      Cash: decimal }
type Place =
    { Id: PlaceId
      Name: string
      Kind: PlaceKind
      Position: Coordinates
      RoadAccess: RoadAccess
      Economy: PlaceEconomy option }
