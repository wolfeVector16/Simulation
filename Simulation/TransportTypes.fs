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
type RouteMode = TravelMode
type RouteEndpoint =
    { PlaceId: PlaceId option
      RoadNodeId: RoadNodeId option
      Position: Coordinates
      Name: string option }
type RouteFailure =
    | MissingRouteEndpoint of PlaceId
    | RoadAccessUnavailable of PlaceId
    | RoadPathUnavailable of RoadNodeId * RoadNodeId
    | PedestrianNetworkUnavailable
    | BikeNetworkUnavailable
    | UnsupportedRouteMode of TravelMode
type RouteGeometry =
    { Polyline: Coordinates list
      DistanceMeters: float }
type RouteLeg =
    { Mode: RouteMode
      From: RouteEndpoint
      To: RouteEndpoint
      Geometry: RouteGeometry
      DistanceMeters: float
      ExpectedMinutes: int
      SegmentId: RoadSegmentId option
      LaneIds: LaneId list
      FromRoadNode: RoadNodeId option
      ToRoadNode: RoadNodeId option
      SegmentTravelMinutes: int
      IntersectionDelayMinutes: int }
type TransportRoute =
    { Id: TransportRouteId
      Mode: RouteMode
      Origin: RouteEndpoint
      Destination: RouteEndpoint
      Legs: RouteLeg list
      Geometry: RouteGeometry
      TotalDistanceMeters: float
      TransitRouteId: TransitRouteId option
      ExpectedMinutes: int
      Reliability: float
      MoneyCost: decimal
      WalkMeters: float
      Safety: float
      Stress: float
      RequiresParking: bool
      TransferCount: int }
type RouteResult<'route> =
    | RouteSucceeded of 'route
    | RouteFailed of RouteFailure
module TransportRoute =
    let segmentIds route =
        route.Legs |> List.choose _.SegmentId

    let laneIds route =
        route.Legs |> List.collect _.LaneIds

    let nodePath route =
        route.Legs
        |> List.choose (fun leg ->
            match leg.FromRoadNode, leg.ToRoadNode with
            | Some fromNode, Some toNode -> Some(fromNode, toNode)
            | _ -> None)
        |> function
            | [] -> []
            | (firstFrom, _) :: pairs -> firstFrom :: (pairs |> List.map snd)

    let segmentTravelMinutes route =
        route.Legs
        |> List.choose (fun leg -> leg.SegmentId |> Option.map (fun _ -> leg.SegmentTravelMinutes))

    let intersectionDelayMinutes route =
        route.Legs
        |> List.choose (fun leg -> leg.SegmentId |> Option.map (fun _ -> leg.IntersectionDelayMinutes))

    let toGeometry route =
        route.Geometry

    let interpolate progress route =
        let points = route.Geometry.Polyline

        match points with
        | [] -> None
        | [ point ] -> Some point
        | _ ->
            let segmentLengths =
                points
                |> List.pairwise
                |> List.map (fun (a, b) ->
                    let dx = a.X - b.X
                    let dy = a.Y - b.Y
                    sqrt (dx * dx + dy * dy))

            let total = segmentLengths |> List.sum
            if total <= 0.0 then
                List.tryHead points
            else
                let target = total * (progress |> max 0.0 |> min 1.0)
                let rec walk walked pairs lengths =
                    match pairs, lengths with
                    | (a, b) :: _, length :: _ when walked + length >= target ->
                        let t = if length <= 0.0001 then 0.0 else (target - walked) / length
                        Some { X = a.X + (b.X - a.X) * t; Y = a.Y + (b.Y - a.Y) * t }
                    | _ :: restPairs, length :: restLengths -> walk (walked + length) restPairs restLengths
                    | _ -> List.tryLast points

                walk 0.0 (points |> List.pairwise) segmentLengths
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
[<RequireQualifiedAccess>]
type MovingEntityKind =
    | Pedestrian of SimId
    | Cyclist of SimId
    | Vehicle of VehicleId
    | TransitVehicle of TransitVehicleId
    | EmergencyResponder of VehicleId
    | FreightVehicle of VehicleId
    | ServiceVehicle of VehicleId
[<RequireQualifiedAccess>]
type MovementStatus =
    | Planned
    | Waiting
    | InProgress
    | Queued
    | WaitingAtIntersection
    | Blocked
    | Delayed
    | Completed
    | Canceled
    | Failed
type VehiclePosition =
    | OnRoadSegment of RoadSegmentId * LaneId option * progress: float
    | WaitingAtIntersection of RoadNodeId * LaneId option
    | ParkedAt of ParkingZoneId option * PlaceId option
    | AtStop of TransitStopId
    | OffNetwork
    | CompletedTripPosition
type VehicleStatus =
    | VehicleNotStarted
    | VehicleMoving
    | VehicleWaitingAtIntersection
    | VehicleQueued
    | VehicleParked
    | VehicleCompleted
    | VehicleCanceled
    | VehicleFailed
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
type MovementState =
    { Id: MovementId
      Kind: MovingEntityKind
      TripId: TransportTripId
      RouteId: TransportRouteId
      Route: TransportRoute
      CurrentLegIndex: int
      DistanceOnLegMeters: float
      TotalDistanceMeters: float
      Progress: float
      CurrentPosition: Coordinates
      PreviousPosition: Coordinates option
      HeadingRadians: float option
      CurrentSpeedKph: float
      Status: MovementStatus
      StartedAt: SimTime
      ExpectedArrival: SimTime
      DelaySeconds: int
      Occupants: int option }
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
    | MovementCompleted of MovementId * TransportTripId
    | MovementBlocked of MovementId * TransportTripId
    | MovementFailed of MovementId * TransportTripId
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
type MovingEntityView =
    { MovementId: MovementId
      EntityKind: MovingEntityKind
      VehicleId: VehicleId option
      SimId: SimId option
      TripId: TransportTripId
      Mode: TravelMode
      CurrentPosition: Coordinates
      PreviousPosition: Coordinates option
      HeadingRadians: float option
      SpeedKph: float
      Status: MovementStatus
      Progress: float
      DelaySeconds: int
      RoutePreview: Coordinates list }
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
      MovingEntities: MovingEntityView list
      Vehicles: MovingEntityView list
      Pedestrians: MovingEntityView list
      TransitVehicles: MovingEntityView list
      RoadSegmentTrafficViews: RoadSegmentTrafficView list
      IntersectionTrafficViews: IntersectionTrafficView list
      Events: TrafficVisualizationEvent list
      Metrics: TrafficFrameMetrics }
type TrafficFrameDiff =
    { Tick: TickId
      AddedVehicles: MovingEntityView list
      UpdatedVehicles: MovingEntityView list
      RemovedVehicles: MovementId list
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
      Movements: Map<MovementId, MovementState>
      Vehicles: Map<VehicleId, VehicleState>
      Incidents: Map<TransportIncidentId, TransportIncident>
      AccessByNeighborhood: Map<NeighborhoodId, AccessProfile>
      SegmentCongestion: Map<RoadSegmentId, float>
      TravelTimeReliability: Map<PlaceId * PlaceId, float>
      RecentEvents: TransportEvent list
      Metrics: TransportMetrics }
