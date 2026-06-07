namespace Simulation.Domain

open System

type RoadNode =
    { Id: RoadNodeId
      Position: Coordinates }
type TravelMode =
    | Walk
    | Bike
    | PrivateCar
    | TaxiOrRideshare
    | Bus
    | Tram
    | Metro
    | RegionalRail
    | FreightTruck
    | EmergencyVehicle
    | ServiceVehicle
    | DeliveryVehicle
    | SchoolBus
    | Paratransit
type RoadClass =
    | Freeway
    | LocalStreet
    | Collector
    | Arterial
    | Highway
    | IndustrialRoad
    | TransitMall
    | Alley
    | ServiceRoad
    | PedestrianPath
    | BikePath
    | TransitCorridor
    | FreightCorridor
type ParkingRule =
    | NoParking
    | FreeOnStreet
    | MeteredParking of hourlyCost: decimal
    | PermitOnly
    | LoadingOnly
    | GarageAccessOnly
type BikeFacility =
    | NoBikeFacility
    | SharedLane
    | PaintedBikeLane
    | ProtectedBikeLane
    | OffStreetTrail
type RoadRestriction =
    | NoTrucks
    | WeightLimitTons of float
    | HeightLimitMeters of float
    | NoThroughTraffic
    | BusOnlyRestriction
    | EmergencyOnlyRestriction
type LaneType =
    | General
    | LeftTurn
    | RightTurn
    | Through
    | ThroughLeft
    | ThroughRight
    | BusOnly
    | BikeOnly
    | ProtectedBike
    | Parking
    | Loading
    | Shoulder
    | Hov
    | Reversible
    | TramTrack
type LaneDirection =
    | Forward
    | Reverse
type Movement =
    | MoveLeft
    | MoveRight
    | MoveThrough
    | MergeLeft
    | MergeRight
    | UTurn
type Lane =
    { Id: LaneId
      SegmentId: RoadSegmentId
      Direction: LaneDirection
      LaneType: LaneType
      AllowedModes: Set<TravelMode>
      PermittedMovements: Set<Movement>
      LengthMeters: float
      CapacityPerHour: float
      CurrentDensity: float
      CurrentSpeedKph: float
      QueueLength: int
      Blocked: bool }
type SignalPhaseKind =
    | ProtectedLeftPhase
    | PermittedLeftPhase
    | ThroughPhase
    | RightTurnPhase
    | PedestrianCrossingPhase
    | BikeCrossingPhase
    | TransitPriorityPhase
    | EmergencyPreemptionPhase
type SignalPhase =
    { Kind: SignalPhaseKind
      DurationSeconds: int
      Movements: Set<Movement> }
type IntersectionControl =
    | Uncontrolled
    | StopSign
    | AllWayStop
    | Yield
    | Signalized of SignalPlanId
    | AdaptiveSignal
    | Roundabout
    | RampMeter
    | RailroadCrossing
    | PedestrianCrossing
    | TransitPrioritySignal
type Intersection =
    { Node: RoadNodeId
      IncomingLanes: Set<LaneId>
      OutgoingLanes: Set<LaneId>
      PermittedMovements: Map<LaneId, Set<Movement>>
      Control: IntersectionControl
      SignalPhases: SignalPhase list
      CrosswalkQuality: float
      BikeCrossingQuality: float
      CapacityPerMinute: int
      QueueSpillbackRisk: float
      MergeDifficulty: float
      VisibilitySafety: float
      IncidentRisk: float }
type RoadSegment =
    { Id: RoadSegmentId
      Name: string
      From: RoadNodeId
      To: RoadNodeId
      LengthMeters: float
      SpeedKph: float
      IsTwoWay: bool
      CapacityPerMinute: int
      RoadClass: RoadClass
      LaneIds: LaneId list
      ParkingRules: ParkingRule list
      TransitLaneIds: LaneId list
      BikeFacility: BikeFacility
      SidewalkQuality: float
      Grade: float
      SurfaceCondition: float
      Toll: decimal option
      Restrictions: Set<RoadRestriction>
      CurrentIncidents: Set<TransportIncidentId>
      UnderConstruction: bool
      WeatherImpact: float
      NoiseOutput: float
      PollutionOutput: float }
