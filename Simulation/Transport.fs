namespace Simulation

open System
open System.Security.Cryptography
open System.Text
open Simulation.Domain
open Simulation.Measures

module Transport =
    let private stableGuid parts =
        let text = String.concat "|" parts
        let bytes = Encoding.UTF8.GetBytes text
        let hash = SHA256.HashData bytes
        let guidBytes = Array.zeroCreate<byte> 16
        Array.Copy(hash, guidBytes, 16)
        Guid(guidBytes)

    let private tripId seed tick label key =
        TransportTripId(stableGuid [ string seed; string tick; label; key ])

    let private routeId seed tick label key =
        TransportRouteId(stableGuid [ "route"; string seed; string tick; label; key ])

    let private vehicleId seed tick label key =
        VehicleId(stableGuid [ "vehicle"; string seed; string tick; label; key ])

    let private movementId seed tick label key =
        MovementId(stableGuid [ "movement"; string seed; string tick; label; key ])

    let private clampInt low high value =
        value |> max low |> min high

    let private minutesAtSpeed speedKph meters =
        if speedKph <= 0.0 then
            Int32.MaxValue
        else
            max 1 (int (Math.Ceiling((meters / 1000.0) / speedKph * 60.0)))

    let private placeName world placeId =
        world.Map.Places
        |> Map.tryFind placeId
        |> Option.map _.Name
        |> Option.defaultValue "unknown"

    let private locationPlace =
        function
        | PlaceRef placeId -> Some placeId
        | _ -> None

    let private distanceMeters world origin destination =
        match Map.tryFind origin world.Map.Places, Map.tryFind destination world.Map.Places with
        | Some a, Some b -> MapGraph.distanceMeters world.Map a.Position b.Position
        | _ -> Double.PositiveInfinity

    let private headingBetween a b =
        Math.Atan2(b.Y - a.Y, b.X - a.X)

    let private movementKindFor mode vehicleId simId =
        match mode, simId with
        | Walk, Some simId -> MovingEntityKind.Pedestrian simId
        | Bike, Some simId -> MovingEntityKind.Cyclist simId
        | Bus, _ | Tram, _ | Metro, _ | RegionalRail, _ | SchoolBus, _ -> MovingEntityKind.TransitVehicle(TransitVehicleId(stableGuid [ "transit-vehicle"; string vehicleId ]))
        | EmergencyVehicle, _ -> MovingEntityKind.EmergencyResponder vehicleId
        | FreightTruck, _ | DeliveryVehicle, _ -> MovingEntityKind.FreightVehicle vehicleId
        | ServiceVehicle, _ -> MovingEntityKind.ServiceVehicle vehicleId
        | _ -> MovingEntityKind.Vehicle vehicleId

    let private movementStatusFromVehicle =
        function
        | VehicleNotStarted -> MovementStatus.Planned
        | VehicleMoving -> MovementStatus.InProgress
        | VehicleWaitingAtIntersection -> MovementStatus.WaitingAtIntersection
        | VehicleQueued -> MovementStatus.Queued
        | VehicleParked
        | VehicleCompleted -> MovementStatus.Completed
        | VehicleCanceled -> MovementStatus.Canceled
        | VehicleFailed -> MovementStatus.Failed

    let private routeLegProgress (route: TransportRoute) legIndex distanceOnLeg =
        let completed =
            route.Legs
            |> List.take (min legIndex route.Legs.Length)
            |> List.sumBy _.DistanceMeters

        if route.TotalDistanceMeters <= 0.0 then
            0.0
        else
            clamp01 ((completed + max 0.0 distanceOnLeg) / route.TotalDistanceMeters)

    let private positionAtLegDistance (route: TransportRoute) legIndex distanceOnLeg =
        let progress = routeLegProgress route legIndex distanceOnLeg
        TransportRoute.interpolate progress route
        |> Option.orElse (route.Geometry.Polyline |> List.tryHead)
        |> Option.defaultValue { X = 0.0; Y = 0.0 }

    let private vehicleLegDistance world route (vehicle: VehicleState) =
        match vehicle.CurrentRouteIndex, vehicle.CurrentPosition with
        | Some index, OnRoadSegment(segmentId, _, progress) ->
            let distance =
                world.Map.RoadSegments
                |> List.tryFind (fun segment -> segment.Id = segmentId)
                |> Option.map (fun segment -> MapGraph.segmentLength world.Map segment * clamp01 progress)
                |> Option.defaultValue 0.0

            index, distance
        | Some index, WaitingAtIntersection _ -> index, route.Legs |> List.tryItem index |> Option.map _.DistanceMeters |> Option.defaultValue 0.0
        | _ -> 0, 0.0

    let private createMovement (world: World) (trip: TransportTrip) (route: TransportRoute) (vehicle: VehicleState) =
        let legIndex, distanceOnLeg = vehicleLegDistance world route vehicle
        let currentPosition = positionAtLegDistance route legIndex distanceOnLeg
        let nextPosition = TransportRoute.interpolate (min 1.0 (routeLegProgress route legIndex distanceOnLeg + 0.001)) route

        { Id = movementId world.Meta.Seed world.Meta.Tick "movement" (string trip.Id)
          Kind = movementKindFor route.Mode vehicle.Id trip.PersonId
          TripId = trip.Id
          RouteId = route.Id
          Route = route
          CurrentLegIndex = legIndex
          DistanceOnLegMeters = distanceOnLeg
          TotalDistanceMeters = route.TotalDistanceMeters
          Progress = routeLegProgress route legIndex distanceOnLeg
          CurrentPosition = currentPosition
          PreviousPosition = None
          HeadingRadians = nextPosition |> Option.map (fun position -> headingBetween currentPosition position)
          CurrentSpeedKph = vehicle.CurrentSpeedKph
          Status = movementStatusFromVehicle vehicle.Status
          StartedAt = { Day = world.Day; MinuteOfDay = world.MinuteOfDay }
          ExpectedArrival = { Day = world.Day; MinuteOfDay = normalizeMinute (world.MinuteOfDay + route.ExpectedMinutes) }
          DelaySeconds = vehicle.DelayMinutes * 60
          Occupants = vehicle.Occupants }

    let classifyIntersectionMovement world nodeId (previousSegment: RoadSegment option) (nextSegment: RoadSegment) =
        TransportRouting.classifyIntersectionMovement world nodeId previousSegment nextSegment

    let intersectionDelayMinutes world mode nodeId previousSegment nextSegment =
        TransportRouting.intersectionDelayMinutes world mode nodeId previousSegment nextSegment

    let private firstParkingNear world destination =
        world.Transport.ParkingZones
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.filter (fun zone -> zone.NearPlace = Some destination)
        |> Seq.sortBy (fun zone -> zone.PricePerHour, zone.AverageSearchMinutes)
        |> Seq.tryHead

    let private transitRouteServing world origin destination =
        let stopPlaces =
            world.Transport.TransitStops
            |> Map.toSeq
            |> Seq.choose (fun (stopId, stop) -> stop.Place |> Option.map (fun placeId -> placeId, stopId))
            |> Map.ofSeq

        match Map.tryFind origin stopPlaces, Map.tryFind destination stopPlaces with
        | Some originStop, Some destinationStop ->
            world.Transport.TransitRoutes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.tryFind (fun (route: TransitRoute) ->
                let stops = route.Stops
                match List.tryFindIndex ((=) originStop) stops, List.tryFindIndex ((=) destinationStop) stops with
                | Some fromIndex, Some toIndex -> fromIndex < toIndex || toIndex < fromIndex
                | _ -> false)
        | _ -> None

    let private neighborhoodForHousehold world householdId =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.tryFind (fun (_, unit) -> Set.contains householdId unit.Occupants)
        |> Option.map (fun (_, unit) -> unit.Neighborhood)

    let private accessForHousehold world household =
        neighborhoodForHousehold world household.Id
        |> Option.bind (fun neighborhoodId -> Map.tryFind neighborhoodId world.Transport.AccessByNeighborhood)

    let private availableModes world (sim: Sim) (household: Household) origin destination =
        let directMeters = distanceMeters world origin destination
        let access = accessForHousehold world household

        let walkLimit = if sim.LifeStage = Child then 1800.0 else 3200.0
        let bikeSafety = access |> Option.map _.BikeSafety |> Option.defaultValue 0.35

        [ if directMeters <= walkLimit then Walk
          if directMeters <= 9000.0 && bikeSafety > 0.45 && sim.LifeStage <> Child then Bike
          if sim.LifeStage <> Child && (household.TransportationAccess >= 0.55 || household.Assets > 5000m) then PrivateCar
          if transitRouteServing world origin destination |> Option.isSome then Bus ]
        |> Set.ofList

    let private driverProfile (sim: Sim) =
        { Aggressiveness = clamp01 (0.25 + (1.0 - sim.Personality.Agreeableness) * 0.40 + sim.Personality.Ambition * 0.15)
          Patience = clamp01 (0.30 + sim.Personality.Agreeableness * 0.35 + sim.Personality.RoutinePreference * 0.20)
          Familiarity = clamp01 (0.45 + sim.Personality.RoutinePreference * 0.35)
          RiskTolerance = clamp01 (0.25 + sim.Personality.Openness * 0.25 + (1.0 - sim.Personality.Neuroticism) * 0.20)
          LawCompliance = clamp01 (0.45 + sim.Personality.Conscientiousness * 0.45)
          StressLevel = clamp01 (1.0 - sim.Happiness)
          Urgency = clamp01 (0.25 + sim.Personality.Ambition * 0.55)
          HighwayAversion = clamp01 (0.15 + sim.Personality.Neuroticism * 0.40)
          TollAversion = clamp01 (0.20 + sim.Personality.Frugality * 0.60)
          RerouteTendency = clamp01 (0.25 + sim.Personality.Openness * 0.35)
          WalkingToleranceMeters = 900.0 + sim.Personality.Openness * 1800.0
          TransitTolerance = clamp01 (0.35 + sim.Personality.Frugality * 0.35 + sim.Personality.RoutinePreference * 0.15) }

    let private tripPurposeFromSimple purpose =
        match purpose with
        | ToWork
        | ToHome -> WorkTrip
        | ToSchool
        | FromSchool -> SchoolTrip
        | ToDaycare
        | FromDaycare -> SchoolPickupDropoffTrip
        | ToShopping _ -> ShoppingTrip
        | ToErrand -> InstitutionalAppointmentTrip
        | ToLeisure -> RecreationTrip

    let private deadlineForSimple (sim: Sim) purpose =
        match purpose, sim.Job, sim.School with
        | ToWork, Some job, _ -> Some job.StartMinute
        | ToSchool, _, Some school -> Some school.StartMinute
        | ToDaycare, _, Some school -> Some school.StartMinute
        | _ -> None

    let private privateCarRoute world seed tick tripKey origin destination deadline (sim: Sim) _household =
        TransportRouting.roadRoute world PrivateCar origin destination
        |> Option.map (fun roadRoute ->
            let parking = firstParkingNear world destination
            let parkingPressure =
                parking
                |> Option.map (fun zone -> float zone.Occupied / max 1.0 (float zone.Capacity))
                |> Option.defaultValue 0.85

            let parkingMinutes = parking |> Option.map _.AverageSearchMinutes |> Option.defaultValue 12
            let parkingCost = parking |> Option.map (fun zone -> zone.PricePerHour) |> Option.defaultValue 4m
            let congestion = roadRoute.Segments |> List.map (TransportRouting.congestionFor world) |> List.append [ 0.0 ] |> List.average
            let expected = int (Math.Ceiling roadRoute.TotalMinutes) + parkingMinutes
            let lateRisk =
                deadline
                |> Option.map (fun deadline -> normalizeMinute (world.MinuteOfDay + expected) - deadline)
                |> Option.defaultValue 0

            let reasons =
                [ if sim.Dependents.Length > 0 then NeedsChildPickup
                  if lateRisk > -10 then DeadlinePressure
                  if transitRouteServing world origin destination |> Option.exists (fun route -> route.Reliability < 0.70) then TransitUnreliable
                  if parkingPressure > 0.82 then ParkingUnavailable
                  if parkingCost > 6m then ParkingTooExpensive
                  if congestion > 0.55 then HeavyCongestion ]

            let route =
                { Id = routeId seed tick "private-car" tripKey
                  Mode = PrivateCar
                  Origin = roadRoute.Origin
                  Destination = roadRoute.Destination
                  Legs = roadRoute.Legs
                  Geometry = roadRoute.Geometry
                  TotalDistanceMeters = roadRoute.TotalDistanceMeters
                  TransitRouteId = None
                  ExpectedMinutes = expected
                  Reliability = clamp01 (0.90 - congestion * 0.35 - parkingPressure * 0.18)
                  MoneyCost = parkingCost + decimal (roadRoute.TotalMinutes * 0.11)
                  WalkMeters = roadRoute.AccessMeters + (parking |> Option.map _.WalkingDistanceMeters |> Option.defaultValue 300.0)
                  Safety = clamp01 (0.80 - congestion * 0.15)
                  Stress = clamp01 (congestion * 0.45 + parkingPressure * 0.35 + sim.Personality.Neuroticism * 0.20)
                  RequiresParking = true
                  TransferCount = 0 }

            route, roadRoute.TotalMinutes + float parkingMinutes, reasons)

    let private busRoute world seed tick tripKey origin destination deadline (sim: Sim) =
        transitRouteServing world origin destination
        |> Option.bind (fun transit ->
            TransportRouting.roadRoute world Bus origin destination
            |> Option.map (fun roadRoute ->
                let trafficPenalty = if transit.DedicatedRightOfWay then 1.0 else 1.0 + (roadRoute.Segments |> List.map (TransportRouting.congestionFor world) |> List.append [ 0.0 ] |> List.average) * 0.45
                let wait = max 1 (transit.HeadwayMinutes / 2)
                let dwell = max 2 (transit.Stops.Length * 1)
                let expected = int (Math.Ceiling(roadRoute.TotalMinutes * trafficPenalty)) + wait + dwell
                let arrival = normalizeMinute (world.MinuteOfDay + expected)
                let lateRisk = deadline |> Option.map (fun d -> arrival - d) |> Option.defaultValue 0

                let reasons =
                    [ if transit.Reliability < 0.70 then TransitUnreliable
                      if world.MinuteOfDay < transit.ServiceStartMinute || world.MinuteOfDay > transit.ServiceEndMinute then TransitUnavailable
                      if lateRisk > -5 then DeadlinePressure
                      if transit.Crowding > 0.80 then MissedConnectionRisk ]

                let route =
                    { Id = routeId seed tick "bus" tripKey
                      Mode = Bus
                      Origin = roadRoute.Origin
                      Destination = roadRoute.Destination
                      Legs = roadRoute.Legs
                      Geometry = roadRoute.Geometry
                      TotalDistanceMeters = roadRoute.TotalDistanceMeters
                      TransitRouteId = Some transit.Id
                      ExpectedMinutes = expected
                      Reliability = clamp01 (transit.Reliability - transit.Crowding * 0.15)
                      MoneyCost = transit.Fare
                      WalkMeters = roadRoute.AccessMeters + 350.0
                      Safety = clamp01 (0.72 + transit.Reliability * 0.15)
                      Stress = clamp01 ((1.0 - transit.Reliability) * 0.45 + transit.Crowding * 0.35 + sim.Personality.Neuroticism * 0.20)
                      RequiresParking = false
                      TransferCount = 0 }

                route, float expected, reasons))

    let private simpleModeRoute world seed tick tripKey mode origin destination (sim: Sim) =
        match mode with
        | Walk ->
            TransportRouting.roadRoute world Walk origin destination
            |> Option.map (fun roadRoute ->
                let driver = driverProfile sim
                let expected = int (Math.Ceiling roadRoute.TotalMinutes)
                let extraReasons = [ if roadRoute.TotalDistanceMeters > driver.WalkingToleranceMeters then MobilityLimitation ]
                let route =
                    { Id = routeId seed tick "walk" tripKey
                      Mode = Walk
                      Origin = roadRoute.Origin
                      Destination = roadRoute.Destination
                      Legs = roadRoute.Legs
                      Geometry = roadRoute.Geometry
                      TotalDistanceMeters = roadRoute.TotalDistanceMeters
                      TransitRouteId = None
                      ExpectedMinutes = expected
                      Reliability = 0.78
                      MoneyCost = 0m
                      WalkMeters = roadRoute.TotalDistanceMeters
                      Safety = 0.62
                      Stress = clamp01 (0.38 * 0.45 + sim.Personality.Neuroticism * 0.25)
                      RequiresParking = false
                      TransferCount = 0 }

                route, roadRoute.TotalMinutes, extraReasons)
        | Bike ->
            None
        | _ ->
            None

    let private chooseModeAndRoute world seed tick tripKey (sim: Sim) household origin destination _ deadline available =
        let candidates =
            available
            |> Seq.sort
            |> Seq.truncate world.Performance.MaxRouteAlternativesPerTrip
            |> Seq.choose (fun mode ->
                match mode with
                | PrivateCar -> privateCarRoute world seed tick tripKey origin destination deadline sim household
                | Bus -> busRoute world seed tick tripKey origin destination deadline sim
                | Walk
                | Bike -> simpleModeRoute world seed tick tripKey mode origin destination sim
                | _ -> None
                |> Option.map (fun (route, minutes, reasons) ->
                    let deadlinePenalty =
                        deadline
                        |> Option.map (fun d -> max 0 (normalizeMinute (world.MinuteOfDay + route.ExpectedMinutes) - d) |> float)
                        |> Option.defaultValue 0.0

                    let generalizedCost =
                        minutes
                        + (float route.MoneyCost * 0.35)
                        + ((1.0 - route.Reliability) * 28.0)
                        + (route.Stress * 18.0)
                        + (deadlinePenalty * 2.0)

                    mode, route, reasons, generalizedCost))
            |> Seq.sortBy (fun (_, route, _, cost) -> cost, route.ExpectedMinutes)
            |> Seq.toList

        candidates |> List.tryHead

    let private simTransportDemand world (simId, sim: Sim) =
        match sim.Location with
        | InTransit trip ->
            match Map.tryFind sim.Household world.Households with
            | None -> None
            | Some household ->
                let activeExists =
                    world.Transport.Trips
                    |> Map.toSeq
                    |> Seq.exists (fun (_, transportTrip) -> transportTrip.PersonId = Some simId && transportTrip.Status = InProgress)

                if activeExists then
                    None
                else
                    let origin = trip.Origin
                    let destination = trip.Destination
                    let available = availableModes world sim household origin destination

                    if available.IsEmpty then
                        None
                    else
                        let key =
                            match simId with
                            | SimId id -> id.ToString("N")

                        let purpose = tripPurposeFromSimple trip.Purpose
                        let deadline = deadlineForSimple sim trip.Purpose
                        let tripId = tripId world.Meta.Seed world.Meta.Tick "person-trip" key
                        let chosen = chooseModeAndRoute world world.Meta.Seed world.Meta.Tick key sim household origin destination purpose deadline available

                        chosen
                        |> Option.map (fun (mode, route, reasons, _) ->
                            let transportTrip =
                                { Id = tripId
                                  PersonId = Some simId
                                  HouseholdId = Some sim.Household
                                  Purpose = purpose
                                  Origin = PlaceRef origin
                                  Destination = PlaceRef destination
                                  DeadlineMinute = deadline
                                  AvailableModes = available
                                  ChosenMode = Some mode
                                  ModeChoiceReasons = reasons
                                  PlannedRoute = Some route
                                  CurrentRoute = Some route
                                  FallbackModes = available |> Set.remove mode |> Set.toList
                                  ToleranceForDelayMinutes = if purpose = WorkTrip || purpose = SchoolTrip then 5 else 20
                                  WillingnessToReroute = driverProfile sim |> _.RerouteTendency
                                  Stress = route.Stress
                                  Status = InProgress
                                  ChainIndex = 0
                                  ChainLength = if sim.Dependents.IsEmpty then 1 else 2 }

                            let driver = driverProfile sim
                            let routeSegmentIds = TransportRoute.segmentIds route
                            let firstSegmentId = routeSegmentIds |> List.tryHead
                            let firstLaneId =
                                firstSegmentId
                                |> Option.bind (fun segmentId ->
                                    world.Map.RoadSegments
                                    |> List.tryFind (fun segment -> segment.Id = segmentId)
                                    |> Option.bind (fun segment -> segment.LaneIds |> List.tryHead))

                            let currentSpeed =
                                firstSegmentId
                                |> Option.bind (fun segmentId -> world.Map.RoadSegments |> List.tryFind (fun segment -> segment.Id = segmentId))
                                |> Option.map (fun segment -> TransportRouting.segmentEffectiveSpeedKph world segment)
                                |> Option.defaultValue 0.0

                            let vehicle =
                                { Id = vehicleId world.Meta.Seed world.Meta.Tick "person-vehicle" key
                                  Trip = tripId
                                  Mode = mode
                                  CurrentPosition =
                                    firstSegmentId
                                    |> Option.map (fun segmentId -> OnRoadSegment(segmentId, firstLaneId, 0.0))
                                    |> Option.defaultValue OffNetwork
                                  PreviousPosition = None
                                  CurrentSpeedKph = currentSpeed
                                  CurrentRouteIndex = firstSegmentId |> Option.map (fun _ -> 0)
                                  Status = if firstSegmentId.IsSome then VehicleMoving else VehicleCompleted
                                  CurrentLane = firstLaneId
                                  NextRequiredMovement = Some MoveRight
                                  DistanceToManeuverMeters = routeSegmentIds.Length |> float |> (*) 500.0
                                  Driver = driver
                                  MissedManeuvers = 0
                                  DelayMinutes = 0
                                  Occupants = Some 1 }

                            transportTrip, vehicle, route, reasons)
        | _ -> None

    let private completeArrivedTrips world =
        let inTransitPeople =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (fun (simId, sim) -> match sim.Location with InTransit _ -> Some simId | _ -> None)
            |> Set.ofSeq

        world.Transport.Trips
        |> Map.toSeq
        |> Seq.choose (fun (tripId, trip) ->
            match trip.Status, trip.PersonId with
            | InProgress, Some simId when not (Set.contains simId inTransitPeople) ->
                let delay =
                    match trip.DeadlineMinute, trip.CurrentRoute with
                    | Some deadline, Some _ -> max 0 (normalizeMinute (world.MinuteOfDay) - deadline)
                    | _ -> 0

                let events =
                    [ TripCompleted tripId
                      if delay > trip.ToleranceForDelayMinutes then
                          ArrivedLate(simId, trip.Purpose, delay) ]

                Some(tripId, { trip with Status = Completed }, events)
            | _ -> None)
        |> Seq.toList

    let private parkingEvents world (trip: TransportTrip) (route: TransportRoute) =
        if not route.RequiresParking then
            []
        else
            match locationPlace trip.Destination |> Option.bind (fun place -> firstParkingNear world place) with
            | None -> [ ParkingSearchStarted trip.Id; ParkingFailed trip.Id ]
            | Some zone ->
                let pressure = float zone.Occupied / max 1.0 (float zone.Capacity)
                if pressure > 0.94 then
                    [ ParkingSearchStarted trip.Id; ParkingFailed trip.Id ]
                else
                    [ ParkingSearchStarted trip.Id; ParkingFound(trip.Id, zone.Id) ]

    let private laneBehaviorEvents world (vehicle: VehicleState) (route: TransportRoute) =
        match vehicle.CurrentLane, TransportRoute.laneIds route |> List.tryLast with
        | Some currentLane, Some targetLane when currentLane <> targetLane ->
            let congestion =
                TransportRoute.segmentIds route
                |> List.map (fun segmentId -> world.Transport.SegmentCongestion |> Map.tryFind segmentId |> Option.defaultValue 0.0)
                |> List.append [ 0.0 ]
                |> List.average

            let mergePressure = congestion * (1.0 - vehicle.Driver.Patience) + vehicle.Driver.StressLevel * 0.25

            if mergePressure > 0.58 then
                [ LaneChangeFailed(vehicle.Id, currentLane, targetLane) ]
            else
                [ LaneChanged(vehicle.Id, currentLane, targetLane) ]
        | _ -> []

    let private routeSegmentLane world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.bind (fun segment -> segment.LaneIds |> List.tryHead)

    let private routeSegmentLength world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.map (fun segment -> MapGraph.segmentLength world.Map segment)
        |> Option.defaultValue Double.PositiveInfinity

    let private routeSegmentSpeed world segmentId =
        world.Map.RoadSegments
        |> List.tryFind (fun segment -> segment.Id = segmentId)
        |> Option.map (fun segment -> TransportRouting.segmentEffectiveSpeedKph world segment)
        |> Option.defaultValue 0.0

    let private routeIntersectionAfterSegment route index =
        TransportRoute.nodePath route
        |> List.tryItem (index + 1)

    let private completeVehicle world (trip: TransportTrip) (route: TransportRoute) (vehicle: VehicleState) previousPosition =
        let destination = locationPlace trip.Destination

        let parking =
            if route.RequiresParking then
                destination |> Option.bind (fun place -> firstParkingNear world place) |> Option.map _.Id
            else
                None

        { vehicle with
            PreviousPosition = previousPosition
            CurrentPosition = if route.RequiresParking then ParkedAt(parking, destination) else CompletedTripPosition
            CurrentSpeedKph = 0.0
            CurrentRouteIndex = None
            Status = if route.RequiresParking then VehicleParked else VehicleCompleted
            CurrentLane = None
            DelayMinutes = 0 }

    let private enterRouteSegment world route index (vehicle: VehicleState) previousPosition =
        match TransportRoute.segmentIds route |> List.tryItem index with
        | None ->
            { vehicle with
                PreviousPosition = previousPosition
                CurrentPosition = CompletedTripPosition
                CurrentSpeedKph = 0.0
                CurrentRouteIndex = None
                Status = VehicleCompleted
                CurrentLane = None
                DelayMinutes = 0 }
        | Some segmentId ->
            let lane = routeSegmentLane world segmentId

            { vehicle with
                PreviousPosition = previousPosition
                CurrentPosition = OnRoadSegment(segmentId, lane, 0.0)
                CurrentSpeedKph = routeSegmentSpeed world segmentId
                CurrentRouteIndex = Some index
                Status = VehicleMoving
                CurrentLane = lane
                DelayMinutes = 0 }

    let private advanceVehicle minutes world transport (vehicle: VehicleState) =
        match Map.tryFind vehicle.Trip transport.Trips, vehicle.CurrentRouteIndex with
        | Some trip, Some index ->
            match trip.CurrentRoute with
            | None -> vehicle
            | Some route ->
                match vehicle.Status, vehicle.CurrentPosition with
                | VehicleWaitingAtIntersection, WaitingAtIntersection _ ->
                    let remaining = max 0 (vehicle.DelayMinutes - minutes)

                    if remaining > 0 then
                        { vehicle with
                            PreviousPosition = Some vehicle.CurrentPosition
                            DelayMinutes = remaining
                            CurrentSpeedKph = 0.0 }
                    else
                        let nextIndex = index + 1

                        if nextIndex >= (TransportRoute.segmentIds route).Length then
                            completeVehicle world trip route vehicle (Some vehicle.CurrentPosition)
                        else
                            enterRouteSegment world route nextIndex vehicle (Some vehicle.CurrentPosition)
                | VehicleMoving, OnRoadSegment(segmentId, laneId, progress) ->
                    let speed = routeSegmentSpeed world segmentId
                    let length = routeSegmentLength world segmentId

                    if Double.IsInfinity length || length <= 0.0 || speed <= 0.0 then
                        { vehicle with
                            PreviousPosition = Some vehicle.CurrentPosition
                            CurrentSpeedKph = 0.0
                            Status = VehicleQueued }
                    else
                        let metersThisTick = speed * 1000.0 / 60.0 * float minutes
                        let nextProgress = progress + metersThisTick / length

                        if nextProgress < 1.0 then
                            { vehicle with
                                PreviousPosition = Some vehicle.CurrentPosition
                                CurrentPosition = OnRoadSegment(segmentId, laneId, clamp01 nextProgress)
                                CurrentSpeedKph = speed
                                Status = VehicleMoving }
                        else
                            let nextIndex = index + 1

                            if nextIndex >= (TransportRoute.segmentIds route).Length then
                                completeVehicle world trip route vehicle (Some vehicle.CurrentPosition)
                            else
                                let delay =
                                    TransportRoute.intersectionDelayMinutes route
                                    |> List.tryItem nextIndex
                                    |> Option.defaultValue 0

                                if delay > 0 then
                                    let intersectionNode = routeIntersectionAfterSegment route index

                                    { vehicle with
                                        PreviousPosition = Some vehicle.CurrentPosition
                                        CurrentPosition =
                                            intersectionNode
                                            |> Option.map (fun nodeId -> WaitingAtIntersection(nodeId, laneId))
                                            |> Option.defaultValue OffNetwork
                                        CurrentSpeedKph = 0.0
                                        Status = VehicleWaitingAtIntersection
                                        DelayMinutes = delay }
                                else
                                    enterRouteSegment world route nextIndex vehicle (Some vehicle.CurrentPosition)
                | _ -> vehicle
        | _ -> vehicle

    let private updateVehicleMovement minutes world transport =
        let vehicles =
            transport.Vehicles
            |> Map.map (fun _ vehicle ->
                match vehicle.Status with
                | VehicleCompleted
                | VehicleParked
                | VehicleCanceled
                | VehicleFailed -> vehicle
                | _ -> advanceVehicle minutes world transport vehicle)

        { transport with Vehicles = vehicles }

    let private updateTripsAndEvents _ world =
        let completed = completeArrivedTrips world

        let tripsAfterCompletions =
            (world.Transport.Trips, completed)
            ||> List.fold (fun trips (tripId, trip, _) -> Map.add tripId trip trips)

        let demand =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (simTransportDemand { world with Transport = { world.Transport with Trips = tripsAfterCompletions } })
            |> Seq.toList

        let trips =
            (tripsAfterCompletions, demand)
            ||> List.fold (fun trips (trip, _, _, _) -> Map.add trip.Id trip trips)

        let vehicles =
            (world.Transport.Vehicles, demand)
            ||> List.fold (fun vehicles (_, vehicle, _, _) -> Map.add vehicle.Id vehicle vehicles)

        let movements =
            (world.Transport.Movements, demand)
            ||> List.fold (fun movements ((trip: TransportTrip), (vehicle: VehicleState), (route: TransportRoute), _) ->
                let movement = createMovement world trip route vehicle
                Map.add movement.Id movement movements)

        let lateStartEvents (trip: TransportTrip) (route: TransportRoute) =
            match trip.DeadlineMinute with
            | Some deadline ->
                let delay = max 0 (normalizeMinute (world.MinuteOfDay + route.ExpectedMinutes) - deadline)

                if delay > trip.ToleranceForDelayMinutes then
                    [ TripDelayed(trip.Id, delay)
                      match trip.PersonId with
                      | Some simId -> ArrivedLate(simId, trip.Purpose, delay)
                      | None -> () ]
                else
                    []
            | None -> []

        let startedEvents =
            demand
            |> List.collect (fun (trip, vehicle, route, _) ->
                [ TripPlanned trip.Id
                  TripStarted trip.Id
                  ModeChosen(trip.Id, route.Mode)
                  RouteChosen(trip.Id, route.Id) ]
                @ parkingEvents world trip route
                @ laneBehaviorEvents world vehicle route
                @ lateStartEvents trip route)

        let completedEvents = completed |> List.collect (fun (_, _, events) -> events)

        { world.Transport with
            Trips = trips
            Movements = movements
            Vehicles = vehicles
            RecentEvents = completedEvents @ startedEvents }

    let private updateLaneState world transport =
        let demandBySegment =
            transport.Vehicles
            |> Map.toSeq
            |> Seq.choose (fun (_, vehicle) ->
                match vehicle.Status, vehicle.CurrentPosition with
                | VehicleMoving, OnRoadSegment(segmentId, _, _)
                | VehicleQueued, OnRoadSegment(segmentId, _, _)
                | VehicleWaitingAtIntersection, OnRoadSegment(segmentId, _, _) -> Some segmentId
                | _ -> None)
            |> Seq.countBy id
            |> Map.ofSeq

        let segmentById = world.Map.RoadSegments |> List.map (fun segment -> segment.Id, segment) |> Map.ofList

        let congestion =
            world.Map.RoadSegments
            |> List.map (fun segment ->
                let demand = demandBySegment |> Map.tryFind segment.Id |> Option.defaultValue 0 |> float
                let capacity = max 1.0 (float segment.CapacityPerMinute * 15.0)
                segment.Id, clamp01 (demand / capacity + if segment.UnderConstruction then 0.25 else 0.0))
            |> Map.ofList

        let lanes =
            transport.Lanes
            |> Map.map (fun _ lane ->
                let segment = Map.tryFind lane.SegmentId segmentById
                let c = congestion |> Map.tryFind lane.SegmentId |> Option.defaultValue 0.0
                let baseSpeed = segment |> Option.map _.SpeedKph |> Option.defaultValue lane.CurrentSpeedKph
                let speed = baseSpeed * (1.0 - min 0.80 (c * 0.65))

                { lane with
                    CurrentDensity = c
                    CurrentSpeedKph = max 3.0 speed
                    QueueLength = int (c * 20.0)
                    Blocked = c > 0.95 })

        { transport with
            Lanes = lanes
            SegmentCongestion = congestion }

    let private updateParkingState transport =
        let activeCarArrivals =
            transport.Trips
            |> Map.toSeq
            |> Seq.choose (fun (_, trip) ->
                match trip.Status, trip.CurrentRoute, locationPlace trip.Destination with
                | InProgress, Some route, Some destination when route.RequiresParking -> Some destination
                | _ -> None)
            |> Seq.countBy id
            |> Map.ofSeq

        let parking =
            transport.ParkingZones
            |> Map.map (fun _ zone ->
                let added =
                    zone.NearPlace
                    |> Option.bind (fun placeId -> activeCarArrivals |> Map.tryFind placeId)
                    |> Option.defaultValue 0

                { zone with
                    Occupied = clampInt 0 zone.Capacity (zone.Occupied + added)
                    AverageSearchMinutes = clampInt 1 45 (zone.AverageSearchMinutes + if added > 0 && zone.Occupied > zone.Capacity * 8 / 10 then 2 else 0) })

        { transport with ParkingZones = parking }

    let private updateAccessMetrics world transport =
        let averageCongestion =
            transport.SegmentCongestion
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.append [ 0.0 ]
            |> Seq.average

        let parkingPressure =
            transport.ParkingZones
            |> Map.toSeq
            |> Seq.map (fun (_, zone) -> float zone.Occupied / max 1.0 (float zone.Capacity))
            |> Seq.append [ 0.0 ]
            |> Seq.average

        let transitReliability =
            transport.TransitRoutes
            |> Map.toSeq
            |> Seq.map (fun (_, route) -> route.Reliability * (1.0 - route.Crowding * 0.25))
            |> Seq.append [ 0.65 ]
            |> Seq.average
            |> clamp01

        let accessByNeighborhood =
            world.Neighborhoods
            |> Map.map (fun _ neighborhood ->
                let congestionPenalty = averageCongestion * 0.30
                let parkingPenalty = parkingPressure * 0.12
                let walkSafety = clamp01 (neighborhood.Walkability * 0.70 + neighborhood.Safety * 0.30)
                let bikeSafety = clamp01 (walkSafety * 0.55 + (1.0 - neighborhood.Pollution) * 0.20 + neighborhood.TransitAccess * 0.15)
                let job = clamp01 (neighborhood.EmploymentAccess * 0.55 + neighborhood.TransitAccess * 0.30 + (1.0 - congestionPenalty) * 0.15)
                let school = clamp01 (neighborhood.SchoolAccess * 0.65 + walkSafety * 0.25 + transitReliability * 0.10)
                let food = clamp01 (neighborhood.Walkability * 0.50 + neighborhood.TransitAccess * 0.25 + (1.0 - parkingPenalty) * 0.25)

                { JobAccess = job
                  SchoolAccess = school
                  HealthcareAccess = clamp01 (neighborhood.HealthAccess * 0.70 + transitReliability * 0.20 + walkSafety * 0.10)
                  FoodAccess = food
                  SocialAccess = clamp01 (neighborhood.SocialCohesion * 0.35 + walkSafety * 0.35 + transitReliability * 0.30)
                  EmergencyAccess = clamp01 (neighborhood.Safety * 0.45 + (1.0 - averageCongestion) * 0.45 + neighborhood.ServiceQuality * 0.10)
                  FreightAccess = clamp01 (neighborhood.EmploymentAccess * 0.45 + (1.0 - averageCongestion) * 0.45 + (1.0 - neighborhood.Pollution) * 0.10)
                  ParkingAccess = clamp01 (1.0 - parkingPressure)
                  TransitReliability = transitReliability
                  WalkSafety = walkSafety
                  BikeSafety = bikeSafety
                  OpportunityAccess = clamp01 ((job + school + food + transitReliability + walkSafety) / 5.0) })

        let lateArrivals =
            transport.RecentEvents
            |> List.sumBy (function ArrivedLate _ -> 1 | _ -> 0)

        let metrics =
            { AverageCongestion = averageCongestion
              AverageTravelReliability = transitReliability * 0.45 + (1.0 - averageCongestion) * 0.55
              AverageParkingPressure = parkingPressure
              TransitTrust = transitReliability
              FreightReliability = clamp01 (1.0 - averageCongestion * 0.60)
              EmergencyResponseRisk = clamp01 (averageCongestion * 0.65 + (1.0 - transitReliability) * 0.10)
              LateArrivalsToday = transport.Metrics.LateArrivalsToday + lateArrivals
              FailedLaneChangesToday = transport.Metrics.FailedLaneChangesToday + (transport.RecentEvents |> List.sumBy (function LaneChangeFailed _ -> 1 | _ -> 0))
              MissedTransfersToday = transport.Metrics.MissedTransfersToday + (transport.RecentEvents |> List.sumBy (function MissedTransfer _ -> 1 | _ -> 0))
              ParkingFailuresToday = transport.Metrics.ParkingFailuresToday + (transport.RecentEvents |> List.sumBy (function ParkingFailed _ -> 1 | _ -> 0)) }

        { transport with
            AccessByNeighborhood = accessByNeighborhood
            Metrics = metrics }

    let private applyAccessFeedback world transport =
        let neighborhoods =
            world.Neighborhoods
            |> Map.map (fun neighborhoodId neighborhood ->
                match Map.tryFind neighborhoodId transport.AccessByNeighborhood with
                | None -> neighborhood
                | Some access ->
                    { neighborhood with
                        TransitAccess = clamp01 (neighborhood.TransitAccess * 0.80 + access.TransitReliability * 0.20)
                        EmploymentAccess = clamp01 (neighborhood.EmploymentAccess * 0.82 + access.JobAccess * 0.18)
                        Walkability = clamp01 (neighborhood.Walkability * 0.88 + access.WalkSafety * 0.12)
                        RentPressure = clamp01 (neighborhood.RentPressure + max 0.0 (access.OpportunityAccess - 0.62) * 0.010)
                        Pollution = clamp01 (neighborhood.Pollution + transport.Metrics.AverageCongestion * 0.006) })

        let households =
            world.Households
            |> Map.map (fun _ household ->
                let neighborhoodAccess =
                    neighborhoodForHousehold world household.Id
                    |> Option.bind (fun neighborhoodId -> Map.tryFind neighborhoodId transport.AccessByNeighborhood)

                match neighborhoodAccess with
                | None -> household
                | Some access ->
                    { household with
                        TransportationAccess = clamp01 (household.TransportationAccess * 0.92 + access.OpportunityAccess * 0.08)
                        Stability = clamp01 (household.Stability - max 0.0 (0.45 - access.OpportunityAccess) * 0.015) })

        let indicators =
            { world.City.Indicators with
                Traffic = clamp01 (world.City.Indicators.Traffic * 0.45 + transport.Metrics.AverageCongestion * 0.55)
                Pollution = clamp01 (world.City.Indicators.Pollution + transport.Metrics.AverageCongestion * 0.020) }

        { world with
            Transport = transport
            Neighborhoods = neighborhoods
            Households = households
            City = { world.City with Indicators = indicators } }

    let tick minutes world =
        let transport =
            updateTripsAndEvents minutes world
            |> updateVehicleMovement minutes world
            |> updateLaneState world
            |> updateParkingState
            |> updateAccessMetrics world

        let world =
            { world with Transport = transport }
            |> MovementSystem.tick minutes

        let routeCalculations =
            world.Transport.RecentEvents
            |> List.sumBy (function RouteChosen _ -> 1 | _ -> 0)

        let world =
            { world with
                PerformanceDiagnostics =
                    { world.PerformanceDiagnostics with
                        RouteCalculations = world.PerformanceDiagnostics.RouteCalculations + routeCalculations
                        CacheMisses = world.PerformanceDiagnostics.CacheMisses + routeCalculations
                        TripsProcessed = world.Transport.Trips.Count } }

        applyAccessFeedback world world.Transport

module TrafficVisualization =
    open Simulation.Domain
    open System

    let private renderPosition (coordinates: Coordinates) : VehicleRenderPosition =
        { RenderX = coordinates.X; RenderY = coordinates.Y; RenderZ = None }

    let private visualStatusFromMovement (status: MovementStatus) =
        match status with
        | MovementStatus.Planned
        | MovementStatus.Waiting -> StoppedVisual
        | MovementStatus.InProgress
        | MovementStatus.Delayed -> Moving
        | MovementStatus.Queued -> QueuedVisual
        | MovementStatus.WaitingAtIntersection
        | MovementStatus.Blocked -> WaitingAtIntersectionVisual
        | MovementStatus.Completed -> CompletedVisual
        | MovementStatus.Canceled
        | MovementStatus.Failed -> HiddenVisual

    let private isActiveMovement (movement: MovementState) =
        match movement.Status with
        | MovementStatus.Completed
        | MovementStatus.Canceled
        | MovementStatus.Failed -> false
        | _ -> true

    let private vehicleIdOfKind =
        function
        | MovingEntityKind.Vehicle vehicleId
        | MovingEntityKind.EmergencyResponder vehicleId
        | MovingEntityKind.FreightVehicle vehicleId
        | MovingEntityKind.ServiceVehicle vehicleId -> Some vehicleId
        | _ -> None

    let private simIdOfKind =
        function
        | MovingEntityKind.Pedestrian simId
        | MovingEntityKind.Cyclist simId -> Some simId
        | _ -> None

    let private currentLeg (movement: MovementState) =
        movement.Route.Legs |> List.tryItem movement.CurrentLegIndex

    let private currentSegmentId (movement: MovementState) =
        movement |> currentLeg |> Option.bind _.SegmentId

    let private currentIntersectionId (movement: MovementState) =
        match movement.Status, currentLeg movement with
        | MovementStatus.WaitingAtIntersection, Some leg -> leg.ToRoadNode
        | MovementStatus.Blocked, Some leg -> leg.ToRoadNode
        | _ -> None

    let private movingEntityView (movement: MovementState) : MovingEntityView =
        { MovementId = movement.Id
          EntityKind = movement.Kind
          VehicleId = vehicleIdOfKind movement.Kind
          SimId = simIdOfKind movement.Kind
          TripId = movement.TripId
          Mode = movement.Route.Mode
          CurrentPosition = movement.CurrentPosition
          PreviousPosition = movement.PreviousPosition
          HeadingRadians = movement.HeadingRadians
          SpeedKph = movement.CurrentSpeedKph
          Status = movement.Status
          Progress = movement.Progress
          DelaySeconds = movement.DelaySeconds
          RoutePreview = movement.Route.Geometry.Polyline }

    let private allMovements (world: World) : MovementState list =
        world.Transport.Movements
        |> Map.toSeq
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.filter isActiveMovement
        |> Seq.toList

    let getMovingEntityView (world: World) (movementId: MovementId) : MovingEntityView option =
        world.Transport.Movements
        |> Map.tryFind movementId
        |> Option.filter isActiveMovement
        |> Option.map movingEntityView

    let getVehicleView (world: World) (vehicleId: VehicleId) : VehicleView option =
        world.Transport.Movements
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.tryFind (fun movement -> isActiveMovement movement && vehicleIdOfKind movement.Kind = Some vehicleId)
        |> Option.map (fun movement ->
            { VehicleId = vehicleId
              TripId = Some movement.TripId
              Mode = movement.Route.Mode
              SegmentId = currentSegmentId movement
              LaneId = movement.Route.Legs |> List.tryItem movement.CurrentLegIndex |> Option.bind (fun leg -> leg.LaneIds |> List.tryHead)
              IntersectionId = currentIntersectionId movement
              Position = renderPosition movement.CurrentPosition
              PreviousPosition = movement.PreviousPosition |> Option.map renderPosition
              ProgressAlongSegment =
                match currentLeg movement with
                | Some leg when leg.DistanceMeters > 0.0 -> Some(Math.Clamp(movement.DistanceOnLegMeters / leg.DistanceMeters, 0.0, 1.0))
                | Some _ -> Some 0.0
                | None -> None
              HeadingRadians = movement.HeadingRadians
              SpeedKph = movement.CurrentSpeedKph
              Status = visualStatusFromMovement movement.Status
              RouteIndex = Some movement.CurrentLegIndex
              Occupancy = movement.Occupants })

    let private allMovingEntityViews (world: World) : MovingEntityView list =
        allMovements world |> List.map movingEntityView

    let getVehiclesOnRoadSegment (world: World) segmentId : MovingEntityView list =
        allMovements world
        |> List.filter (fun movement -> currentSegmentId movement = Some segmentId && (vehicleIdOfKind movement.Kind).IsSome)
        |> List.map movingEntityView

    let getVehiclesAtIntersection (world: World) intersectionId : MovingEntityView list =
        allMovements world
        |> List.filter (fun movement -> currentIntersectionId movement = Some intersectionId && (vehicleIdOfKind movement.Kind).IsSome)
        |> List.map movingEntityView

    let private roadSegmentView (world: World) (movements: MovementState list) (segment: RoadSegment) : RoadSegmentTrafficView =
        let onSegment =
            movements
            |> List.filter (fun movement -> currentSegmentId movement = Some segment.Id && (vehicleIdOfKind movement.Kind).IsSome)

        let averageSpeed =
            onSegment
            |> List.map _.CurrentSpeedKph
            |> List.append [ 0.0 ]
            |> List.average

        let queueLength =
            world.Transport.Lanes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun lane -> lane.SegmentId = segment.Id)
            |> Seq.sumBy _.QueueLength

        let startPosition =
            world.Map.RoadNodes
            |> Map.tryFind segment.From
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let endPosition =
            world.Map.RoadNodes
            |> Map.tryFind segment.To
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let view: RoadSegmentTrafficView =
            { SegmentId = segment.Id
              StartPosition = startPosition
              EndPosition = endPosition
              RoadClass = segment.RoadClass
              LaneCount = segment.LaneIds.Length
              IsTwoWay = segment.IsTwoWay
              ActiveVehicleCount = onSegment.Length
              AverageSpeedKph = averageSpeed
              Congestion = world.Transport.SegmentCongestion |> Map.tryFind segment.Id |> Option.defaultValue 0.0
              QueueLength = queueLength
              IsClosed = segment.UnderConstruction || not segment.CurrentIncidents.IsEmpty }

        view

    let private intersectionView (world: World) (movements: MovementState list) nodeId (intersection: Intersection) : IntersectionTrafficView =
        let waiting =
            movements
            |> List.filter (fun movement -> currentIntersectionId movement = Some nodeId && (vehicleIdOfKind movement.Kind).IsSome)

        let averageDelay =
            waiting
            |> List.map (fun movement -> float movement.DelaySeconds)
            |> List.append [ 0.0 ]
            |> List.average

        let position =
            world.Map.RoadNodes
            |> Map.tryFind nodeId
            |> Option.map (fun node -> renderPosition node.Position)
            |> Option.defaultValue { RenderX = 0.0; RenderY = 0.0; RenderZ = None }

        let view: IntersectionTrafficView =
            { IntersectionId = nodeId
              Position = position
              WaitingVehicleCount = waiting.Length
              AverageDelaySeconds = averageDelay
              ControlType = intersection.Control
              CurrentPhase = if intersection.SignalPhases.IsEmpty then None else Some 0 }

        view

    let private frameMetrics (world: World) (vehicles: MovingEntityView list) : TrafficFrameMetrics =
        let moving = vehicles |> List.filter (fun vehicle -> vehicle.Status = MovementStatus.InProgress || vehicle.Status = MovementStatus.Delayed)
        let waiting = vehicles |> List.filter (fun vehicle -> vehicle.Status = MovementStatus.WaitingAtIntersection || vehicle.Status = MovementStatus.Queued || vehicle.Status = MovementStatus.Blocked)
        let completed =
            world.Transport.Movements
            |> Map.toSeq
            |> Seq.sumBy (fun (_, movement) ->
                if (vehicleIdOfKind movement.Kind).IsSome && movement.Status = MovementStatus.Completed then 1 else 0)

        { ActiveVehicleCount = vehicles.Length
          MovingVehicleCount = moving.Length
          WaitingVehicleCount = waiting.Length
          ParkedVehicleCount = 0
          CompletedVehicleCount = completed
          AverageVehicleSpeedKph = vehicles |> List.map _.SpeedKph |> List.append [ 0.0 ] |> List.average
          AverageCongestion = world.Transport.Metrics.AverageCongestion }

    let getTrafficFrame (world: World) : TrafficFrame =
        let movements = allMovements world
        let movingEntities = movements |> List.map movingEntityView
        let vehicles = movingEntities |> List.filter (fun entity -> entity.VehicleId.IsSome)
        let pedestrians =
            movingEntities
            |> List.filter (fun entity ->
                match entity.EntityKind with
                | MovingEntityKind.Pedestrian _ -> true
                | _ -> false)

        let transitVehicles =
            movingEntities
            |> List.filter (fun entity ->
                match entity.EntityKind with
                | MovingEntityKind.TransitVehicle _ -> true
                | _ -> false)

        let frame: TrafficFrame =
            { Tick = TickId world.Meta.Tick
              SimTime = { Day = world.Day; MinuteOfDay = world.MinuteOfDay }
              MovingEntities = movingEntities
              Vehicles = vehicles
              Pedestrians = pedestrians
              TransitVehicles = transitVehicles
              RoadSegmentTrafficViews =
                world.Map.RoadSegments
                |> List.sortBy _.Id
                |> List.map (roadSegmentView world movements)
              IntersectionTrafficViews =
                world.Transport.Intersections
                |> Map.toSeq
                |> Seq.sortBy fst
                |> Seq.map (fun (nodeId, intersection) -> intersectionView world movements nodeId intersection)
                |> Seq.toList
              Events = []
              Metrics = frameMetrics world vehicles }

        frame

    let getRenderableRoute (world: World) (tripId: TransportTripId) : RenderableRoute option =
        world.Transport.Trips
        |> Map.tryFind tripId
        |> Option.bind (fun trip ->
            trip.CurrentRoute
            |> Option.map (fun route ->
                let segments =
                    route.Legs
                    |> List.choose (fun leg ->
                        leg.SegmentId
                        |> Option.map (fun segmentId ->
                            let fromPosition =
                                leg.Geometry.Polyline
                                |> List.tryHead
                                |> Option.map renderPosition
                                |> Option.defaultValue (renderPosition leg.From.Position)

                            let toPosition =
                                leg.Geometry.Polyline
                                |> List.tryLast
                                |> Option.map renderPosition
                                |> Option.defaultValue (renderPosition leg.To.Position)

                            { SegmentId = segmentId
                              FromPosition = fromPosition
                              ToPosition = toPosition
                              ExpectedTravelMinutes = leg.SegmentTravelMinutes
                              ExpectedIntersectionDelayMinutes = leg.IntersectionDelayMinutes }))

                let renderable: RenderableRoute =
                    { TripId = tripId
                      RouteId = route.Id
                      Mode = route.Mode
                      Segments = segments
                      ExpectedMinutes = route.ExpectedMinutes }

                renderable))

    let diffTrafficFrames (previous: TrafficFrame) (current: TrafficFrame) : TrafficFrameDiff =
        let previousEntities = previous.MovingEntities |> List.map (fun entity -> entity.MovementId, entity) |> Map.ofList
        let currentEntities = current.MovingEntities |> List.map (fun entity -> entity.MovementId, entity) |> Map.ofList

        let added =
            current.MovingEntities
            |> List.filter (fun entity -> not (Map.containsKey entity.MovementId previousEntities))

        let updated =
            current.MovingEntities
            |> List.filter (fun entity ->
                previousEntities
                |> Map.tryFind entity.MovementId
                |> Option.exists (fun previous -> previous <> entity))

        let removed =
            previous.MovingEntities
            |> List.filter (fun entity -> not (Map.containsKey entity.MovementId currentEntities))
            |> List.map _.MovementId

        let previousRoads = previous.RoadSegmentTrafficViews |> List.map (fun road -> road.SegmentId, road) |> Map.ofList
        let changedRoads =
            current.RoadSegmentTrafficViews
            |> List.filter (fun road -> previousRoads |> Map.tryFind road.SegmentId |> Option.exists (fun prev -> prev <> road))

        let previousIntersections = previous.IntersectionTrafficViews |> List.map (fun intersection -> intersection.IntersectionId, intersection) |> Map.ofList
        let changedIntersections =
            current.IntersectionTrafficViews
            |> List.filter (fun intersection -> previousIntersections |> Map.tryFind intersection.IntersectionId |> Option.exists (fun prev -> prev <> intersection))

        let vehicleEvents =
            [ for movementId in removed do
                  match Map.tryFind movementId previousEntities with
                  | Some entity ->
                      match entity.VehicleId with
                      | Some vehicleId -> VehicleCompletedTrip(vehicleId, entity.TripId)
                      | None -> ()
                  | None -> () ]

        { Tick = current.Tick
          AddedVehicles = added
          UpdatedVehicles = updated
          RemovedVehicles = removed
          ChangedRoadSegments = changedRoads
          ChangedIntersections = changedIntersections
          Events =
            vehicleEvents
            @ (changedRoads |> List.map (fun road -> RoadTrafficStateChanged road.SegmentId))
            @ (changedIntersections |> List.map (fun intersection -> IntersectionStateChanged intersection.IntersectionId)) }

