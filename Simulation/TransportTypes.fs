namespace Simulation.Domain

open System

type TransitStop =
    { Id: TransitStopId
      Name: string
      Place: PlaceId option
      Node: RoadNodeId option
      Position: Coordinates
      Accessibility: float
      PerceivedSafety: float }
type TransitRoute =
    { Id: TransitRouteId
      Name: string
      Mode: TravelMode
      Stops: TransitStopId list
      HeadwayMinutes: int
      ServiceStartMinute: int
      ServiceEndMinute: int
      Fare: decimal
      Capacity: int
      Reliability: float
      DedicatedRightOfWay: bool
      SignalPriority: bool
      Crowding: float }
type ParkingZone =
    { Id: ParkingZoneId
      Name: string
      NearPlace: PlaceId option
      Capacity: int
      Occupied: int
      PricePerHour: decimal
      AverageSearchMinutes: int
      PermitRequired: bool
      IllegalParkingRisk: float
      WalkingDistanceMeters: float }
type DriverProfile =
    { Aggressiveness: float
      Patience: float
      Familiarity: float
      RiskTolerance: float
      LawCompliance: float
      StressLevel: float
      Urgency: float
      HighwayAversion: float
      TollAversion: float
      RerouteTendency: float
      WalkingToleranceMeters: float
      TransitTolerance: float }
type TransportTripPurpose =
    | WorkTrip
    | SchoolTrip
    | ShoppingTrip
    | HealthcareTrip
    | CaregivingTrip
    | SocialTrip
    | RecreationTrip
    | FreightDeliveryTrip
    | EmergencyResponseTrip
    | InstitutionalAppointmentTrip
    | JobInterviewTrip
    | HousingSearchTrip
    | SchoolPickupDropoffTrip
type LocationRef =
    | PlaceRef of PlaceId
    | NodeRef of RoadNodeId
    | StopRef of TransitStopId
    | ParkingRef of ParkingZoneId
type IntersectionMovement =
    | Straight
    | LeftTurnMovement
    | RightTurnMovement
    | UTurnMovement
    | MergeMovement
    | DivergeMovement
    | UnknownMovement
type TransportRoute =
    { Id: TransportRouteId
      Mode: TravelMode
      SegmentIds: RoadSegmentId list
      LaneIds: LaneId list
      NodePath: RoadNodeId list
      SegmentTravelMinutes: int list
      IntersectionDelayMinutes: int list
      TransitRouteId: TransitRouteId option
      ExpectedMinutes: int
      Reliability: float
      MoneyCost: decimal
      WalkMeters: float
      Safety: float
      Stress: float
      RequiresParking: bool
      TransferCount: int }
type TripStatus =
    | Planned
    | InProgress
    | Completed
    | Canceled
    | Failed
type TransportTrip =
    { Id: TransportTripId
      PersonId: SimId option
      HouseholdId: HouseholdId option
      Purpose: TransportTripPurpose
      Origin: LocationRef
      Destination: LocationRef
      DeadlineMinute: int option
      AvailableModes: Set<TravelMode>
      ChosenMode: TravelMode option
      ModeChoiceReasons: DecisionReason list
      PlannedRoute: TransportRoute option
      CurrentRoute: TransportRoute option
      FallbackModes: TravelMode list
      ToleranceForDelayMinutes: int
      WillingnessToReroute: float
      Stress: float
      Status: TripStatus
      ChainIndex: int
      ChainLength: int }
type VehicleState =
    { Id: VehicleId
      Trip: TransportTripId
      Mode: TravelMode
      CurrentPosition: VehiclePosition
      PreviousPosition: VehiclePosition option
      CurrentSpeedKph: float
      CurrentRouteIndex: int option
      Status: VehicleStatus
      CurrentLane: LaneId option
      NextRequiredMovement: Movement option
      DistanceToManeuverMeters: float
      Driver: DriverProfile
      MissedManeuvers: int
      DelayMinutes: int
      Occupants: int option }
and VehiclePosition =
    | OnRoadSegment of RoadSegmentId * LaneId option * progress: float
    | WaitingAtIntersection of RoadNodeId * LaneId option
    | ParkedAt of ParkingZoneId option * PlaceId option
    | AtStop of TransitStopId
    | OffNetwork
    | CompletedTripPosition
and VehicleStatus =
    | VehicleNotStarted
    | VehicleMoving
    | VehicleWaitingAtIntersection
    | VehicleQueued
    | VehicleParked
    | VehicleCompleted
    | VehicleCanceled
    | VehicleFailed
type TransportIncidentKind =
    | Crash
    | StalledVehicle
    | PoliceStop
    | RoadConstruction
    | LaneClosure
    | SignalFailure
    | TransitBreakdown
    | CrowdingDelay
    | WeatherSlowdown
    | Flooding
    | SnowIce
    | EventTraffic
    | EmergencyClosure
type TransportIncident =
    { Id: TransportIncidentId
      Kind: TransportIncidentKind
      Segment: RoadSegmentId option
      Lane: LaneId option
      StartedDay: int
      StartedMinute: int
      DurationMinutes: int
      CapacityReduction: float
      Severity: float
      Description: string }
type TransportEvent =
    | TripPlanned of TransportTripId
    | TripStarted of TransportTripId
    | ModeChosen of TransportTripId * TravelMode
    | RouteChosen of TransportTripId * TransportRouteId
    | LaneChanged of VehicleId * LaneId * LaneId
    | LaneChangeFailed of VehicleId * LaneId * LaneId
    | ExitMissed of VehicleId * RoadNodeId
    | RouteReplanned of TransportTripId
    | TripDelayed of TransportTripId * delayMinutes: int
    | TripCanceled of TransportTripId
    | TripCompleted of TransportTripId
    | ArrivedLate of SimId * TransportTripPurpose * delayMinutes: int
    | MissedTransfer of TransportTripId * TransitStopId
    | TransitVehicleDelayed of TransitRouteId * delayMinutes: int
    | TransitVehicleCrowded of TransitRouteId
    | BusBunched of TransitRouteId
    | ParkingSearchStarted of TransportTripId
    | ParkingFound of TransportTripId * ParkingZoneId
    | ParkingFailed of TransportTripId
    | IllegalParkingOccurred of TransportTripId
    | CrashOccurred of RoadSegmentId
    | RoadBlocked of RoadSegmentId
    | SignalFailed of RoadNodeId
    | ConstructionStarted of RoadSegmentId
    | ConstructionEnded of RoadSegmentId
    | EmergencyResponseDelayed of InstitutionId * delayMinutes: int
    | DeliveryDelayed of PlaceId * delayMinutes: int
    | CommutePatternChanged of SimId * TravelMode
    | HouseholdVehiclePurchased of HouseholdId
    | HouseholdVehicleSold of HouseholdId
    | TransitTrustChanged of HouseholdId option * delta: float
    | RoadConditionDeclined of RoadSegmentId
    | BikeCrashOccurred of SimId * RoadSegmentId option
    | PedestrianNearMissOccurred of SimId * RoadSegmentId option
type AccessProfile =
    { JobAccess: float
      SchoolAccess: float
      HealthcareAccess: float
      FoodAccess: float
      SocialAccess: float
      EmergencyAccess: float
      FreightAccess: float
      ParkingAccess: float
      TransitReliability: float
      WalkSafety: float
      BikeSafety: float
      OpportunityAccess: float }
type TransportMetrics =
    { AverageCongestion: float
      AverageTravelReliability: float
      AverageParkingPressure: float
      TransitTrust: float
      FreightReliability: float
      EmergencyResponseRisk: float
      LateArrivalsToday: int
      FailedLaneChangesToday: int
      MissedTransfersToday: int
      ParkingFailuresToday: int }
type VehicleRenderPosition =
    { RenderX: float
      RenderY: float
      RenderZ: float option }
type VehicleVisualStatus =
    | Moving
    | WaitingAtIntersectionVisual
    | QueuedVisual
    | ParkedVisual
    | StoppedVisual
    | CompletedVisual
    | HiddenVisual
type VehicleView =
    { VehicleId: VehicleId
      TripId: TransportTripId option
      Mode: TravelMode
      SegmentId: RoadSegmentId option
      LaneId: LaneId option
      IntersectionId: RoadNodeId option
      Position: VehicleRenderPosition
      PreviousPosition: VehicleRenderPosition option
      ProgressAlongSegment: float option
      HeadingRadians: float option
      SpeedKph: float
      Status: VehicleVisualStatus
      RouteIndex: int option
      Occupancy: int option }
type RoadSegmentTrafficView =
    { SegmentId: RoadSegmentId
      StartPosition: VehicleRenderPosition
      EndPosition: VehicleRenderPosition
      RoadClass: RoadClass
      LaneCount: int
      IsTwoWay: bool
      ActiveVehicleCount: int
      AverageSpeedKph: float
      Congestion: float
      QueueLength: int
      IsClosed: bool }
type IntersectionTrafficView =
    { IntersectionId: RoadNodeId
      Position: VehicleRenderPosition
      WaitingVehicleCount: int
      AverageDelaySeconds: float
      ControlType: IntersectionControl
      CurrentPhase: int option }
type TrafficVisualizationEvent =
    | VehicleEnteredSegment of VehicleId * RoadSegmentId
    | VehicleLeftSegment of VehicleId * RoadSegmentId
    | VehicleStoppedAtIntersection of VehicleId * RoadNodeId
    | VehicleStartedMoving of VehicleId
    | VehicleParkedEvent of VehicleId
    | VehicleCompletedTrip of VehicleId * TransportTripId
    | RoadTrafficStateChanged of RoadSegmentId
    | IntersectionStateChanged of RoadNodeId
type TrafficFrameMetrics =
    { ActiveVehicleCount: int
      MovingVehicleCount: int
      WaitingVehicleCount: int
      ParkedVehicleCount: int
      CompletedVehicleCount: int
      AverageVehicleSpeedKph: float
      AverageCongestion: float }
type RenderableRouteSegment =
    { SegmentId: RoadSegmentId
      FromPosition: VehicleRenderPosition
      ToPosition: VehicleRenderPosition
      ExpectedTravelMinutes: int
      ExpectedIntersectionDelayMinutes: int }
type RenderableRoute =
    { TripId: TransportTripId
      RouteId: TransportRouteId
      Mode: TravelMode
      Segments: RenderableRouteSegment list
      ExpectedMinutes: int }
type VehicleMotionView =
    { VehicleId: VehicleId
      PreviousPosition: VehicleRenderPosition option
      CurrentPosition: VehicleRenderPosition
      SegmentId: RoadSegmentId option
      Progress: float option
      SpeedKph: float
      Status: VehicleVisualStatus }
type TrafficFrame =
    { Tick: TickId
      SimTime: SimTime
      Vehicles: VehicleView list
      RoadSegments: RoadSegmentTrafficView list
      Intersections: IntersectionTrafficView list
      TransitVehicles: VehicleView list
      Events: TrafficVisualizationEvent list
      Metrics: TrafficFrameMetrics }
type TrafficFrameDiff =
    { Tick: TickId
      AddedVehicles: VehicleView list
      UpdatedVehicles: VehicleView list
      RemovedVehicles: VehicleId list
      ChangedRoadSegments: RoadSegmentTrafficView list
      ChangedIntersections: IntersectionTrafficView list
      Events: TrafficVisualizationEvent list }
type TransportState =
    { Lanes: Map<LaneId, Lane>
      Intersections: Map<RoadNodeId, Intersection>
      TransitStops: Map<TransitStopId, TransitStop>
      TransitRoutes: Map<TransitRouteId, TransitRoute>
      ParkingZones: Map<ParkingZoneId, ParkingZone>
      Trips: Map<TransportTripId, TransportTrip>
      Vehicles: Map<VehicleId, VehicleState>
      Incidents: Map<TransportIncidentId, TransportIncident>
      AccessByNeighborhood: Map<NeighborhoodId, AccessProfile>
      SegmentCongestion: Map<RoadSegmentId, float>
      TravelTimeReliability: Map<PlaceId * PlaceId, float>
      RecentEvents: TransportEvent list
      Metrics: TransportMetrics }
