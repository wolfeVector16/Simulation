namespace Simulation.Domain

open System

type WealthClass =
    | LowWealth
    | MiddleWealth
    | HighWealth
type BuildingUse =
    | Housing
    | Commerce
    | Industry
    | PublicService
    | Recreation
type BuildingStatus =
    | Vacant
    | Developing
    | Occupied
    | Abandoned
type Building =
    { Name: string
      Use: BuildingUse
      Wealth: WealthClass
      Capacity: int
      Occupants: int
      Jobs: int
      Status: BuildingStatus }
type Parcel =
    { Id: ParcelId
      Name: string
      Zone: ZoneType
      Density: Density
      Position: Coordinates
      Area: float
      Building: Building option
      LandValue: float
      Desirability: float
      Pollution: float
      Crime: float
      FireRisk: float
      Powered: bool
      Watered: bool
      RoadConnected: bool }
