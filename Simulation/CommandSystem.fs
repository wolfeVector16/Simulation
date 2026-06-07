namespace Simulation

open System
open System.Security.Cryptography
open System.Text
open Simulation.Domain
open Simulation.Measures

module CommandSystem =
    type CommandExecutionResult =
        { World: World
          ResolvedCommands: ResolvedCommand list
          Rejections: CommandRejection list
          Events: DomainEvent list }

    let private stableGuid parts =
        let text = String.concat "|" parts
        let bytes = Encoding.UTF8.GetBytes text
        let hash = SHA256.HashData bytes
        let guidBytes = Array.zeroCreate<byte> 16
        Array.Copy(hash, guidBytes, 16)
        Guid(guidBytes)

    let private eventId world label index =
        EventId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; "command"; label; string index ])

    let private roadSegmentId world label index =
        RoadSegmentId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; "road-segment"; label; string index ])

    let private laneId world label index laneIndex =
        LaneId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; "lane"; label; string index; string laneIndex ])

    let private jobId world label index jobIndex =
        JobId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; "job"; label; string index; string jobIndex ])

    let private policeDispatchId world label index =
        PoliceDispatchId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; "police-dispatch"; label; string index ])

    let private emptyRoadSegmentId = RoadSegmentId Guid.Empty

    let private sourceOf command =
        match command with
        | BuildRoad command -> command.Source
        | ModifyRoad command -> command.Source
        | DestroyRoad command -> command.Source
        | BuildBuilding command -> command.Source
        | ModifyBuilding command -> command.Source
        | DestroyBuilding command -> command.Source
        | BuildInstitution command -> command.Source
        | ModifyInstitution command -> command.Source
        | CloseInstitution command -> command.Source
        | ZoneParcels command -> command.Source
        | RezoneParcels command -> command.Source
        | DezoneParcels command -> command.Source
        | BuildTransitRoute command -> command.Source
        | ModifyTransitRoute command -> command.Source
        | RemoveTransitRoute command -> command.Source
        | BuildUtility command -> command.Source
        | ModifyUtility command -> command.Source
        | DestroyUtility command -> command.Source
        | SetBudget command -> command.Source
        | PassPolicy command -> command.Source
        | RepealPolicy command -> command.Source
        | IssueBond command -> command.Source
        | SetTaxRate command -> command.Source
        | EmergencyAction command -> command.Source
        | RepairAsset command -> command.Source
        | CondemnAsset command -> command.Source
        | CreateDistrict command -> command.Source
        | ModifyDistrict command -> command.Source
        | StreetCommand command ->
            match command with
            | MoveActor command -> command.Context.CommandSource
            | InteractWithPerson command -> command.Context.CommandSource
            | InteractWithObject command -> command.Context.CommandSource
            | EnterVehicle command -> command.Context.CommandSource
            | ExitVehicle command -> command.Context.CommandSource
            | DriveVehicle command -> command.Context.CommandSource
            | AttemptUnauthorizedVehicleAccess command -> command.Context.CommandSource
            | EnterBuilding command -> command.Context.CommandSource
            | ExitBuilding command -> command.Context.CommandSource
            | AttemptUnauthorizedEntry command -> command.Context.CommandSource
            | PurchaseItem command -> command.Context.CommandSource
            | AttemptTakeItemWithoutPayment command -> command.Context.CommandSource
            | UseItem command -> command.Context.CommandSource
            | StartConflict command -> command.Context.CommandSource
            | FleeScene command -> command.Context.CommandSource
            | ReportCrime command -> command.Context.CommandSource
            | CallEmergencyService command -> command.Context.CommandSource
            | Trespass command -> command.Context.CommandSource
            | DamageProperty command -> command.Context.CommandSource
            | SurrenderToPolice command -> command.Context.CommandSource
            | DebugTeleportActor command -> command.Context.CommandSource

    let private actorOfStreetCommand command =
        match command with
        | MoveActor command -> command.Context.CommandActor
        | InteractWithPerson command -> command.Context.CommandActor
        | InteractWithObject command -> command.Context.CommandActor
        | EnterVehicle command -> command.Context.CommandActor
        | ExitVehicle command -> command.Context.CommandActor
        | DriveVehicle command -> command.Context.CommandActor
        | AttemptUnauthorizedVehicleAccess command -> command.Context.CommandActor
        | EnterBuilding command -> command.Context.CommandActor
        | ExitBuilding command -> command.Context.CommandActor
        | AttemptUnauthorizedEntry command -> command.Context.CommandActor
        | PurchaseItem command -> command.Context.CommandActor
        | AttemptTakeItemWithoutPayment command -> command.Context.CommandActor
        | UseItem command -> command.Context.CommandActor
        | StartConflict command -> command.Context.CommandActor
        | FleeScene command -> command.Context.CommandActor
        | ReportCrime command -> command.Context.CommandActor
        | CallEmergencyService command -> command.Context.CommandActor
        | Trespass command -> command.Context.CommandActor
        | DamageProperty command -> command.Context.CommandActor
        | SurrenderToPolice command -> command.Context.CommandActor
        | DebugTeleportActor command -> command.Context.CommandActor

    let private contextOfStreetCommand command =
        match command with
        | MoveActor command -> command.Context
        | InteractWithPerson command -> command.Context
        | InteractWithObject command -> command.Context
        | EnterVehicle command -> command.Context
        | ExitVehicle command -> command.Context
        | DriveVehicle command -> command.Context
        | AttemptUnauthorizedVehicleAccess command -> command.Context
        | EnterBuilding command -> command.Context
        | ExitBuilding command -> command.Context
        | AttemptUnauthorizedEntry command -> command.Context
        | PurchaseItem command -> command.Context
        | AttemptTakeItemWithoutPayment command -> command.Context
        | UseItem command -> command.Context
        | StartConflict command -> command.Context
        | FleeScene command -> command.Context
        | ReportCrime command -> command.Context
        | CallEmergencyService command -> command.Context
        | Trespass command -> command.Context
        | DamageProperty command -> command.Context
        | SurrenderToPolice command -> command.Context
        | DebugTeleportActor command -> command.Context

    let private commandCost command =
        match command with
        | BuildRoad command -> command.Cost
        | ModifyRoad command -> command.Cost
        | DestroyRoad command -> command.CleanupCost
        | BuildBuilding command -> command.ConstructionCost
        | ModifyBuilding command -> command.Cost
        | DestroyBuilding command -> command.DemolitionCost
        | BuildInstitution command -> command.Cost
        | ModifyInstitution command -> command.Cost
        | BuildTransitRoute command -> command.Cost
        | ModifyTransitRoute command -> command.Cost
        | BuildUtility command -> command.Cost
        | ModifyUtility command -> command.Cost
        | DestroyUtility command -> command.CleanupCost
        | SetBudget command -> command.MonthlyAmount
        | PassPolicy command -> command.MonthlyCost
        | IssueBond command -> command.Amount
        | EmergencyAction command -> command.Cost
        | RepairAsset command -> command.Cost
        | CloseInstitution _
        | ZoneParcels _
        | RezoneParcels _
        | DezoneParcels _
        | RemoveTransitRoute _
        | RepealPolicy _
        | SetTaxRate _
        | CondemnAsset _
        | CreateDistrict _
        | ModifyDistrict _
        | StreetCommand _ -> 0m

    let private sourceHasSystemAuthority source =
        match source with
        | SimulationSystemCommand _
        | ScenarioScriptCommand _
        | DisasterCommand _
        | InstitutionCommand _
        | HouseholdCommand _
        | DeveloperCommand _
        | ActorCommand _
        | DebugCommand -> true
        | PlayerCommand _ -> false

    let private authorityAllowsCommand mode command =
        match mode, sourceOf command with
        | _, DebugCommand -> true
        | _, source when sourceHasSystemAuthority source -> true
        | ObserverMode, PlayerCommand _ -> false
        | SandboxGodMode, PlayerCommand _ -> true
        | ScenarioDirectorMode, PlayerCommand _ ->
            match command with
            | EmergencyAction _
            | RepairAsset _
            | CondemnAsset _ -> true
            | _ -> false
        | PlannerMode, PlayerCommand _ ->
            match command with
            | BuildRoad _
            | ModifyRoad _
            | ZoneParcels _
            | RezoneParcels _
            | DezoneParcels _
            | BuildBuilding _
            | ModifyBuilding _
            | BuildTransitRoute _
            | ModifyTransitRoute _
            | CreateDistrict _
            | ModifyDistrict _ -> true
            | _ -> false
        | CityManagerMode, PlayerCommand _
        | MayorMode, PlayerCommand _ ->
            match command with
            | BuildRoad _
            | ModifyRoad _
            | DestroyRoad _
            | BuildBuilding _
            | ModifyBuilding _
            | DestroyBuilding _
            | BuildInstitution _
            | ModifyInstitution _
            | CloseInstitution _
            | ZoneParcels _
            | RezoneParcels _
            | DezoneParcels _
            | BuildTransitRoute _
            | ModifyTransitRoute _
            | RemoveTransitRoute _
            | BuildUtility _
            | ModifyUtility _
            | DestroyUtility _
            | SetBudget _
            | PassPolicy _
            | RepealPolicy _
            | IssueBond _
            | SetTaxRate _
            | EmergencyAction _
            | RepairAsset _
            | CondemnAsset _
            | CreateDistrict _
            | ModifyDistrict _ -> true
            | StreetCommand _ -> false
        | StreetLevelActorMode actorId, PlayerCommand _ ->
            match command with
            | StreetCommand command -> actorOfStreetCommand command = actorId
            | _ -> false
        | StreetLevelActorMode actorId, ActorCommand sourceActor ->
            actorId = sourceActor
        | _ -> false

    let private bypassesCost mode source =
        mode = SandboxGodMode
        || match source with
           | DebugCommand
           | DisasterCommand _
           | SimulationSystemCommand _
           | ScenarioScriptCommand _ -> true
           | _ -> false

    let private treasuryRejection world mode command =
        let source = sourceOf command
        let cost = commandCost command

        if cost > world.City.Budget.Treasury && not (bypassesCost mode source) then
            Some(InsufficientFunds(Money cost, Money world.City.Budget.Treasury))
        else
            None

    let private buildingUseAllowed useKind zone =
        match useKind, zone with
        | Housing, ResidentialZone
        | Housing, SingleFamilyResidentialZone
        | Housing, MultifamilyResidentialZone
        | Housing, MixedUseZone -> true
        | Commerce, CommercialZone
        | Commerce, NeighborhoodCommercialZone
        | Commerce, ShoppingCenterZone
        | Commerce, OfficeZone
        | Commerce, MixedUseZone
        | Commerce, TransitOrientedZone -> true
        | Industry, IndustrialZone
        | Industry, WarehouseLogisticsZone -> true
        | PublicService, CivicZone
        | PublicService, SchoolZone
        | PublicService, MedicalZone
        | PublicService, UtilityZone -> true
        | Recreation, ParkZone
        | Recreation, ParkOpenSpaceZone -> true
        | _ -> false

    let private buildingIdForParcel parcelId = BuildingId parcelId

    let private parcelForBuilding (BuildingId parcelId) (world: World) =
        world.City.Parcels
        |> Map.tryFind parcelId
        |> Option.bind (fun parcel -> parcel.Building |> Option.map (fun building -> parcelId, parcel, building))

    let private displacedHouseholdsForBuilding (parcel: Parcel) (building: Building) (world: World) =
        if building.Use <> Housing || building.Occupants <= 0 then
            []
        else
            let contains (left: string) (right: string) =
                left.Contains(right, StringComparison.OrdinalIgnoreCase)
                || right.Contains(left, StringComparison.OrdinalIgnoreCase)

            let directMatches =
                world.Households
                |> Map.toSeq
                |> Seq.choose (fun (householdId, household) ->
                    world.Map.Places
                    |> Map.tryFind household.Home
                    |> Option.bind (fun place ->
                        if contains parcel.Name place.Name || contains building.Name place.Name then Some householdId else None))
                |> Seq.toList

            if directMatches.IsEmpty then
                world.Households
                |> Map.toSeq
                |> Seq.truncate building.Occupants
                |> Seq.map fst
                |> Seq.toList
            else
                directMatches

    let private roadNetworkConnectedAfterRemoving roadSegmentId world =
        let remaining =
            world.Map.RoadSegments
            |> List.filter (fun segment -> segment.Id <> roadSegmentId)

        let nodes =
            remaining
            |> List.collect (fun segment -> [ segment.From; segment.To ])
            |> Set.ofList

        if nodes.Count <= 1 then
            true
        else
            let adjacency =
                remaining
                |> List.collect (fun segment ->
                    [ segment.From, segment.To
                      if segment.IsTwoWay then segment.To, segment.From ])
                |> Seq.groupBy fst
                |> Seq.map (fun (node, edges) -> node, edges |> Seq.map snd |> Set.ofSeq)
                |> Map.ofSeq

            let rec visit seen frontier =
                match frontier with
                | [] -> seen
                | node :: rest ->
                    let next =
                        adjacency
                        |> Map.tryFind node
                        |> Option.defaultValue Set.empty
                        |> Set.filter (fun neighbor -> not (Set.contains neighbor seen))
                        |> Set.toList

                    visit (Set.add node seen) (rest @ next)

            let reachable = visit Set.empty [ nodes |> Seq.head ]
            reachable.Count = nodes.Count

    let rec private placeOfActorLocation world location =
        match location with
        | ActorAtPlace placeId -> Some placeId
        | ActorInsideBuilding buildingId ->
            world.Street.Buildings
            |> Map.tryFind buildingId
            |> Option.map _.Place
        | ActorInsideVehicle vehicleId ->
            world.Street.Vehicles
            |> Map.tryFind vehicleId
            |> Option.bind (fun vehicle -> placeOfActorLocation world vehicle.Location)
        | ActorOnRoadSegment _
        | ActorInTransit _ -> None

    let private distanceBetweenPlaces world a b =
        match Map.tryFind a world.Map.Places, Map.tryFind b world.Map.Places with
        | Some left, Some right -> MapGraph.distanceMeters world.Map left.Position right.Position
        | _ -> Double.PositiveInfinity

    let private isWithinDirectRange world actorLocation targetLocation =
        match placeOfActorLocation world actorLocation, placeOfActorLocation world targetLocation with
        | Some a, Some b -> a = b || distanceBetweenPlaces world a b <= 75.0
        | _ -> actorLocation = targetLocation

    let private connectedPlaces world origin destination =
        if origin = destination then
            true
        else
            let rec visit seen frontier =
                match frontier with
                | [] -> false
                | place :: rest when place = destination -> true
                | place :: rest ->
                    let next =
                        world.Street.PlaceConnections
                        |> Map.tryFind place
                        |> Option.defaultValue Set.empty
                        |> Set.filter (fun candidate -> not (Set.contains candidate seen))
                        |> Set.toList

                    visit (Set.add place seen) (rest @ next)

            visit Set.empty [ origin ]

    let private actorRejections world actorId =
        if Map.containsKey actorId world.Street.Actors then [] else [ EntityNotFound(ActorRef actorId) ]

    let private currentActor world actorId =
        world.Street.Actors |> Map.tryFind actorId

    let private streetCommandFeasibility world command =
        let context = contextOfStreetCommand command
        let actorMissing = actorRejections world context.CommandActor
        let actor = currentActor world context.CommandActor

        let locationInvariant =
            match actor with
            | Some actor when actor.Location <> context.CommandLocation ->
                [ InvariantViolation "Command location does not match actor's authoritative location." ]
            | _ -> []

        let impossible =
            [ yield! actorMissing
              yield! locationInvariant
              match command with
              | MoveActor command ->
                  if not (Map.containsKey command.Destination world.Map.Places) then
                      EntityNotFound(PlaceEntityRef command.Destination)
                  else
                      match actor |> Option.bind (fun a -> placeOfActorLocation world a.Location) with
                      | Some origin when connectedPlaces world origin command.Destination -> ()
                      | Some _ -> InvariantViolation "No connected street route exists between actor and destination."
                      | None -> InvariantViolation "Actor is not at a routable street location."
              | InteractWithPerson command ->
                  match Map.tryFind command.Target world.Street.Actors, actor with
                  | None, _ -> EntityNotFound(ActorRef command.Target)
                  | Some target, Some actor when not (isWithinDirectRange world actor.Location target.Location) ->
                      InvariantViolation "Direct person interaction target is outside physical interaction range."
                  | _ -> ()
              | InteractWithObject command ->
                  if not (actor |> Option.exists (fun actor -> Map.containsKey command.Target actor.Inventory)) then
                      EntityNotFound(ItemRef command.Target)
              | EnterVehicle command ->
                  match Map.tryFind command.Vehicle world.Street.Vehicles, actor with
                  | None, _ -> EntityNotFound(VehicleRef command.Vehicle)
                  | Some vehicle, Some actor when not (isWithinDirectRange world actor.Location vehicle.Location) ->
                      InvariantViolation "Vehicle is outside physical interaction range."
                  | Some vehicle, _ when vehicle.Disabled || vehicle.Access = VehicleDisabled ->
                      InvariantViolation "Cannot enter a disabled vehicle."
                  | _ -> ()
              | ExitVehicle command ->
                  match actor with
                  | Some actor ->
                      match actor.Location, actor.CurrentVehicle with
                      | ActorInsideVehicle _, Some _ -> ()
                      | _ -> InvariantViolation "Actor is not inside a vehicle."
                  | None -> ()
              | DriveVehicle command ->
                  match Map.tryFind command.Vehicle world.Street.Vehicles, actor with
                  | None, _ -> EntityNotFound(VehicleRef command.Vehicle)
                  | Some vehicle, Some actor ->
                      if vehicle.Disabled || vehicle.Access = VehicleDisabled then
                          InvariantViolation "Cannot drive a disabled vehicle."
                      elif vehicle.Controller <> Some actor.Id then
                          InvariantViolation "Actor does not control this vehicle."
                      elif not (Map.containsKey command.Destination world.Map.Places) then
                          EntityNotFound(PlaceEntityRef command.Destination)
                      else
                          match placeOfActorLocation world vehicle.Location with
                          | Some origin when connectedPlaces world origin command.Destination -> ()
                          | Some _ -> InvariantViolation "No connected transport route exists for vehicle destination."
                          | None -> InvariantViolation "Vehicle is not at a routable location."
                  | _ -> ()
              | AttemptUnauthorizedVehicleAccess command ->
                  match Map.tryFind command.Vehicle world.Street.Vehicles, actor with
                  | None, _ -> EntityNotFound(VehicleRef command.Vehicle)
                  | Some vehicle, Some actor when not (isWithinDirectRange world actor.Location vehicle.Location) ->
                      InvariantViolation "Vehicle is outside physical interaction range."
                  | Some vehicle, _ when vehicle.Disabled || vehicle.Access = VehicleDisabled ->
                      InvariantViolation "Cannot attempt access to a disabled vehicle."
                  | _ -> ()
              | EnterBuilding command ->
                  match Map.tryFind command.Building world.Street.Buildings, actor with
                  | None, _ -> EntityNotFound(BuildingRef command.Building)
                  | Some building, Some actor when not (isWithinDirectRange world actor.Location (ActorAtPlace building.Place)) ->
                      InvariantViolation "Building entrance is outside physical interaction range."
                  | Some building, _ when building.Access = BuildingCondemnedAccess || building.Access = BuildingClosed || not building.IsOpen ->
                      InvariantViolation "Building is not physically enterable."
                  | _ -> ()
              | ExitBuilding _ ->
                  match actor with
                  | Some actor ->
                      match actor.Location with
                      | ActorInsideBuilding _ -> ()
                      | _ -> InvariantViolation "Actor is not inside a building."
                  | None -> ()
              | AttemptUnauthorizedEntry command ->
                  match Map.tryFind command.Building world.Street.Buildings, actor with
                  | None, _ -> EntityNotFound(BuildingRef command.Building)
                  | Some building, Some actor when not (isWithinDirectRange world actor.Location (ActorAtPlace building.Place)) ->
                      InvariantViolation "Building entrance is outside physical interaction range."
                  | Some building, _ when building.Access = BuildingCondemnedAccess ->
                      InvariantViolation "Condemned building entry is blocked by hard safety invariant."
                  | _ -> ()
              | Trespass command ->
                  match Map.tryFind command.Building world.Street.Buildings, actor with
                  | None, _ -> EntityNotFound(BuildingRef command.Building)
                  | Some building, Some actor when not (isWithinDirectRange world actor.Location (ActorAtPlace building.Place)) ->
                      InvariantViolation "Building entrance is outside physical interaction range."
                  | Some building, _ when building.Access = BuildingCondemnedAccess ->
                      InvariantViolation "Condemned building entry is blocked by hard safety invariant."
                  | _ -> ()
              | PurchaseItem command ->
                  match actor with
                  | Some actor ->
                      if not (Map.containsKey command.Seller world.Map.Places) then
                          EntityNotFound(PlaceEntityRef command.Seller)
                      elif not (isWithinDirectRange world actor.Location (ActorAtPlace command.Seller)) then
                          InvariantViolation "Seller is outside physical interaction range."
                      else
                          match actor.PersonId |> Option.bind (fun simId -> Map.tryFind simId world.Sims) with
                          | Some sim when sim.Wallet >= command.Item.Price -> ()
                          | Some _ -> InsufficientFunds(Money command.Item.Price, Money 0m)
                          | None -> InvariantViolation "Purchasing actor has no wallet-backed person."
                  | None -> ()
              | AttemptTakeItemWithoutPayment command ->
                  match actor with
                  | Some actor ->
                      if not (Map.containsKey command.FromPlace world.Map.Places) then
                          EntityNotFound(PlaceEntityRef command.FromPlace)
                      elif not (isWithinDirectRange world actor.Location (ActorAtPlace command.FromPlace)) then
                          InvariantViolation "Item source is outside physical interaction range."
                  | None -> ()
              | UseItem command ->
                  match actor with
                  | Some actor when not (Map.containsKey command.Item actor.Inventory) -> EntityNotFound(ItemRef command.Item)
                  | _ -> ()
              | StartConflict command ->
                  match Map.tryFind command.Target world.Street.Actors, actor with
                  | None, _ -> EntityNotFound(ActorRef command.Target)
                  | Some target, Some actor when not (isWithinDirectRange world actor.Location target.Location) ->
                      InvariantViolation "Conflict target is outside physical interaction range."
                  | _ -> ()
              | DamageProperty command ->
                  match command.Target with
                  | BuildingRef buildingId when not (Map.containsKey buildingId world.Street.Buildings) -> EntityNotFound(BuildingRef buildingId)
                  | VehicleRef vehicleId when not (Map.containsKey vehicleId world.Street.Vehicles) -> EntityNotFound(VehicleRef vehicleId)
                  | PlaceEntityRef placeId when not (Map.containsKey placeId world.Map.Places) -> EntityNotFound(PlaceEntityRef placeId)
                  | _ -> ()
              | DebugTeleportActor command ->
                  if command.Context.CommandSource <> DebugCommand then
                      UnauthorizedSource command.Context.CommandSource
              | FleeScene _
              | ReportCrime _
              | CallEmergencyService _
              | SurrenderToPolice _ -> () ]

        if not impossible.IsEmpty then
            Impossible impossible, [], impossible
        else
            match command with
            | AttemptUnauthorizedVehicleAccess command ->
                FeasibleUnauthorized { Reason = "Vehicle access is unauthorized but physically attemptable."; AffectedOwner = Some(VehicleRef command.Vehicle) },
                [ UnauthorizedAction; IllegalAction; WitnessRisk; PoliceResponseRisk; HeatIncreaseRisk; PropertyDamageRisk ],
                []
            | AttemptUnauthorizedEntry command ->
                FeasibleUnauthorized { Reason = "Building entry is unauthorized but physically attemptable."; AffectedOwner = Some(BuildingRef command.Building) },
                [ UnauthorizedAction; IllegalAction; WitnessRisk; PoliceResponseRisk; HeatIncreaseRisk; ReputationRisk ],
                []
            | AttemptTakeItemWithoutPayment _ ->
                FeasibleIllegal { Rule = "Taking goods without payment is an abstract illegal action."; Severity = 0.45 },
                [ UnauthorizedAction; IllegalAction; WitnessRisk; PoliceResponseRisk; HeatIncreaseRisk; BusinessInterruptionRisk ],
                []
            | Trespass command ->
                FeasibleIllegal { Rule = "Trespass is an abstract illegal action."; Severity = 0.25 },
                [ UnauthorizedAction; IllegalAction; WitnessRisk; PoliceResponseRisk ],
                []
            | DamageProperty command ->
                FeasibleHostile { Target = Some command.Target; Severity = command.Severity },
                [ IllegalAction; WitnessRisk; PoliceResponseRisk; PropertyDamageRisk; HeatIncreaseRisk ],
                []
            | StartConflict command ->
                FeasibleHostile { Target = Some(ActorRef command.Target); Severity = 0.35 },
                [ WitnessRisk; PoliceResponseRisk; ReputationRisk ],
                []
            | EnterBuilding command ->
                match Map.tryFind command.Building world.Street.Buildings with
                | Some building when building.Access = BuildingPublic -> FeasibleAuthorized, [], []
                | Some _ ->
                    FeasibleUnauthorized { Reason = "Building access is restricted; use AttemptUnauthorizedEntry to resolve consequences."; AffectedOwner = Some(BuildingRef command.Building) },
                    [ UnauthorizedAction ],
                    []
                | None -> FeasibleAuthorized, [], []
            | EnterVehicle command ->
                match Map.tryFind command.Vehicle world.Street.Vehicles with
                | Some vehicle when vehicle.Access = VehiclePublicAccess || vehicle.Controller = Some context.CommandActor -> FeasibleAuthorized, [], []
                | Some _ ->
                    FeasibleUnauthorized { Reason = "Vehicle access is restricted; use AttemptUnauthorizedVehicleAccess to resolve consequences."; AffectedOwner = Some(VehicleRef command.Vehicle) },
                    [ UnauthorizedAction ],
                    []
                | None -> FeasibleAuthorized, [], []
            | _ -> FeasibleAuthorized, [], []

    let private baseValidation mode world command =
        [ if not (authorityAllowsCommand mode command) then
              UnauthorizedSource(sourceOf command)
          match treasuryRejection world mode command with
          | Some rejection -> rejection
          | None -> () ]

    let validateCommand mode world command =
        let feasibility, commandWarnings, streetRejections =
            match command with
            | StreetCommand streetCommand -> streetCommandFeasibility world streetCommand
            | _ -> FeasibleAuthorized, [], []

        let rejections =
            match command with
            | StreetCommand _ ->
                [ yield! baseValidation mode world command
                  yield! streetRejections ]
            | BuildBuilding command ->
                [ yield! baseValidation mode world (BuildBuilding command)
                  match Map.tryFind command.TargetParcel world.City.Parcels with
                  | None -> EntityNotFound(ParcelRef command.TargetParcel)
                  | Some parcel ->
                      if parcel.Building.IsSome then
                          InvariantViolation $"Parcel {command.TargetParcel} already has a building."

                      if not (buildingUseAllowed command.BuildingUse parcel.Zone) then
                          InvalidZoning(command.TargetParcel, command.RequiredZoning)

                      if command.ExpectedCapacity <= 0 then
                          InvariantViolation "Building capacity must be positive."

                      if not parcel.RoadConnected then
                          InvariantViolation "Parcel must have road access before construction." ]
            | DestroyBuilding command ->
                [ yield! baseValidation mode world (DestroyBuilding command)
                  match parcelForBuilding command.BuildingId world with
                  | None -> EntityNotFound(BuildingRef command.BuildingId)
                  | Some(_, parcel, building) ->
                      let displaced = displacedHouseholdsForBuilding parcel building world

                      match mode, sourceOf (DestroyBuilding command), displaced, command.DisplacementPolicy with
                      | SandboxGodMode, _, _, _ -> ()
                      | _, source, _, _ when sourceHasSystemAuthority source -> ()
                      | _, _, [], _ -> ()
                      | ObserverMode, _, _, _ -> UnauthorizedSource(command.Source)
                      | MayorMode, PlayerCommand _, _ :: _, None
                      | PlannerMode, PlayerCommand _, _ :: _, None -> OccupiedBuildingRequiresDisplacementPlan command.BuildingId
                      | _ -> () ]
            | RezoneParcels command ->
                [ yield! baseValidation mode world (RezoneParcels command)
                  for parcelId in command.ParcelIds do
                      match Map.tryFind parcelId world.City.Parcels with
                      | None -> EntityNotFound(ParcelRef parcelId)
                      | Some parcel when parcel.Zone <> command.FromZone -> InvalidZoning(parcelId, command.FromZone)
                      | Some _ -> ()

                  if command.EffectiveDate.Day < world.Day then
                      ScenarioRuleViolation "Rezone effective date cannot be in the past." ]
            | ZoneParcels command ->
                [ yield! baseValidation mode world (ZoneParcels command)
                  for parcelId in command.ParcelIds do
                      if not (Map.containsKey parcelId world.City.Parcels) then
                          EntityNotFound(ParcelRef parcelId) ]
            | BuildRoad command ->
                [ yield! baseValidation mode world (BuildRoad command)
                  if not (Map.containsKey command.FromNode world.Map.RoadNodes) then
                      EntityNotFound(RoadNodeRef command.FromNode)

                  if not (Map.containsKey command.ToNode world.Map.RoadNodes) then
                      EntityNotFound(RoadNodeRef command.ToNode)

                  if command.Lanes.IsEmpty
                     || command.Lanes |> List.exists (fun lane -> lane.AllowedModes.IsEmpty || lane.PermittedMovements.IsEmpty) then
                      InvalidLaneConfiguration emptyRoadSegmentId

                  if command.SpeedLimit <= 0.0 then
                      InvariantViolation "Road speed limit must be positive." ]
            | DestroyRoad command ->
                [ yield! baseValidation mode world (DestroyRoad command)
                  if not (world.Map.RoadSegments |> List.exists (fun segment -> segment.Id = command.RoadSegmentId)) then
                      EntityNotFound(RoadSegmentRef command.RoadSegmentId)
                  elif not command.RerouteRequired && not (roadNetworkConnectedAfterRemoving command.RoadSegmentId world) then
                      WouldDisconnectRoadNetwork command.RoadSegmentId ]
            | ModifyRoad command ->
                [ yield! baseValidation mode world (ModifyRoad command)
                  if not (world.Map.RoadSegments |> List.exists (fun segment -> segment.Id = command.RoadSegmentId)) then
                      EntityNotFound(RoadSegmentRef command.RoadSegmentId) ]
            | BuildInstitution command ->
                [ yield! baseValidation mode world (BuildInstitution command)
                  if command.Capacity <= 0 then
                      InvariantViolation "Institution capacity must be positive."

                  if not (Map.containsKey command.Neighborhood world.Neighborhoods) then
                      EntityNotFound(UnknownEntityRef "neighborhood") ]
            | ModifyInstitution command ->
                [ yield! baseValidation mode world (ModifyInstitution command)
                  if not (Map.containsKey command.InstitutionId world.Institutions) then
                      EntityNotFound(InstitutionRef command.InstitutionId) ]
            | CloseInstitution command ->
                [ yield! baseValidation mode world (CloseInstitution command)
                  if not (Map.containsKey command.InstitutionId world.Institutions) then
                      EntityNotFound(InstitutionRef command.InstitutionId) ]
            | _ -> baseValidation mode world command

        match rejections with
        | [] ->
            Valid
                { Command = command
                  AuthorityMode = mode
                  Source = sourceOf command
                  Actor =
                      match command with
                      | StreetCommand command -> Some(actorOfStreetCommand command)
                      | _ -> None
                  Feasibility = feasibility
                  CommandWarnings = commandWarnings
                  Warnings = commandWarnings |> List.map string }
        | rejections -> Invalid rejections

    let private genericPreview validation cost =
        { Validation = validation
          ExpectedCost = cost
          ExpectedAffectedHouseholds = []
          ExpectedAffectedJobs = []
          ExpectedTrafficDisruption = 0.0
          ExpectedServiceImpact = 0.0
          ExpectedLandValueEffect = 0.0
          ExpectedDisplacementRisk = 0.0
          Warnings = []
          RequiredFollowUpActions = [] }

    let previewCommand snapshot command =
        let data = WorldSnapshot.value snapshot
        let warnings =
            [ if data.Tick = TickId 0 then
                  "Preview is based on the immutable tick snapshot; full entity feasibility requires the current world." ]

        { genericPreview
            (Valid
                { Command = command
                  AuthorityMode = SandboxGodMode
                  Source = sourceOf command
                  Actor =
                      match command with
                      | StreetCommand command -> Some(actorOfStreetCommand command)
                      | _ -> None
                  Feasibility = FeasibleAuthorized
                  CommandWarnings = []
                  Warnings = warnings })
            (commandCost command) with
            Warnings = warnings }

    let previewCommandForWorld mode world command =
        let validation = validateCommand mode world command

        let preview = genericPreview validation (commandCost command)

        match command with
        | DestroyBuilding command ->
            match parcelForBuilding command.BuildingId world with
            | Some(_, parcel, building) ->
                let displaced = displacedHouseholdsForBuilding parcel building world

                { preview with
                    ExpectedAffectedHouseholds = displaced
                    ExpectedDisplacementRisk = if displaced.IsEmpty then 0.0 else 1.0
                    Warnings =
                        if displaced.IsEmpty then
                            preview.Warnings
                        else
                            "Occupied housing demolition requires displacement handling." :: preview.Warnings
                    RequiredFollowUpActions =
                        if displaced.IsEmpty then [] else [ "Provide relocation or shelter capacity."; "Monitor household stability." ] }
            | None -> preview
        | RezoneParcels command ->
            { preview with
                ExpectedLandValueEffect = if command.ToZone = MixedUseZone || command.ToZone = MultifamilyResidentialZone then 0.08 else 0.02
                ExpectedTrafficDisruption = if command.ToZone = IndustrialZone || command.ToZone = ShoppingCenterZone then 0.10 else 0.03 }
        | BuildRoad command ->
            { preview with
                ExpectedTrafficDisruption = 0.20
                Warnings = if command.AffectedParcels.IsEmpty then preview.Warnings else "Road construction may disrupt parcel access." :: preview.Warnings }
        | _ -> preview

    let private buildingFromCommand command =
        { Name = sprintf "New %A" command.BuildingUse
          Use = command.BuildingUse
          Wealth = MiddleWealth
          Capacity = command.ExpectedCapacity
          Occupants = 0
          Jobs =
            match command.BuildingUse with
            | Commerce
            | Industry
            | PublicService -> max 1 (command.ExpectedCapacity / 2)
            | Housing
            | Recreation -> 0
          Status = Developing }

    let private roadFromCommand world command index =
        let segmentId = roadSegmentId world "build-road" index
        let fromNode = world.Map.RoadNodes[command.FromNode]
        let toNode = world.Map.RoadNodes[command.ToNode]
        let length = MapGraph.distanceMeters world.Map fromNode.Position toNode.Position

        let lanes =
            command.Lanes
            |> List.mapi (fun laneIndex spec ->
                { Id = laneId world "build-road" index laneIndex
                  SegmentId = segmentId
                  Direction = spec.Direction
                  LaneType = spec.LaneType
                  AllowedModes = spec.AllowedModes
                  PermittedMovements = spec.PermittedMovements
                  LengthMeters = length
                  CapacityPerHour = 900.0
                  CurrentDensity = 0.0
                  CurrentSpeedKph = command.SpeedLimit
                  QueueLength = 0
                  Blocked = false })

        let segment =
            { Id = segmentId
              Name = sprintf "Planned %A" command.RoadClass
              From = command.FromNode
              To = command.ToNode
              LengthMeters = length
              SpeedKph = command.SpeedLimit
              IsTwoWay = lanes |> List.exists (fun lane -> lane.Direction = Reverse)
              CapacityPerMinute = max 1 (lanes.Length * 12)
              RoadClass = command.RoadClass
              LaneIds = lanes |> List.map _.Id
              ParkingRules = []
              TransitLaneIds = lanes |> List.filter (fun lane -> lane.LaneType = BusOnly) |> List.map _.Id
              BikeFacility = command.BikeFacilities
              SidewalkQuality = if command.Sidewalks then 0.80 else 0.0
              Grade = 0.0
              SurfaceCondition = 1.0
              Toll = None
              Restrictions = Set.empty
              CurrentIncidents = Set.empty
              UnderConstruction = command.ConstructionTime > 0
              WeatherImpact = 0.0
              NoiseOutput = 0.20
              PollutionOutput = 0.18 }

        segment, lanes

    let private heatUp =
        function
        | NoHeat -> LowHeat
        | LowHeat -> ModerateHeat
        | ModerateHeat -> HighHeat
        | HighHeat
        | CitywideAlert -> CitywideAlert

    let private attemptSucceeds world index actorId targetLabel resolution =
        match resolution with
        | ForceSuccess -> true
        | ForceFailure -> false
        | ResolveByRiskModel ->
            let key = stableGuid [ string world.Meta.Seed; string world.Meta.Tick; string index; string actorId; targetLabel ]
            key.ToByteArray()[0] % 2uy = 0uy

    let private actorPlace world actorId =
        world.Street.Actors
        |> Map.tryFind actorId
        |> Option.bind (fun actor -> placeOfActorLocation world actor.Location)

    let private vehiclePlace world vehicleId =
        world.Street.Vehicles
        |> Map.tryFind vehicleId
        |> Option.bind (fun vehicle -> placeOfActorLocation world vehicle.Location)

    let private nearbyWitnesses world actorId incidentPlace =
        world.Street.Actors
        |> Map.toSeq
        |> Seq.choose (fun (candidateId, candidate) ->
            if candidateId = actorId then
                None
            else
                placeOfActorLocation world candidate.Location
                |> Option.bind (fun witnessPlace ->
                    if witnessPlace = incidentPlace || distanceBetweenPlaces world witnessPlace incidentPlace <= 100.0 then
                        Some candidateId
                    else
                        None))
        |> Seq.sort
        |> Seq.truncate 2
        |> Seq.toList

    let private policeDispatch world index reportedActor incidentPlace sourceEvent =
        let police =
            world.Institutions
            |> Map.toSeq
            |> Seq.filter (fun (_, institution) -> institution.Kind = PoliceInstitution)
            |> Seq.choose (fun (institutionId, institution) ->
                institution.Place
                |> Option.map (fun placeId -> institutionId, institution, placeId, distanceBetweenPlaces world placeId incidentPlace))
            |> Seq.sortBy (fun (_, institution, _, meters) -> meters, -institution.Funding, -institution.StaffLevel)
            |> Seq.tryHead

        match police with
        | None ->
            { Id = policeDispatchId world "street" index
              ReportedActor = reportedActor
              IncidentPlace = Some incidentPlace
              PoliceInstitution = None
              Route = []
              ExpectedResponseMinutes = 999
              Priority = 1
              SourceEvent = Some sourceEvent }
        | Some (institutionId, institution, stationPlace, meters) ->
            let connected = connectedPlaces world stationPlace incidentPlace
            let fundingFactor = if institution.Funding >= 700m then 0.75 elif institution.Funding <= 250m then 1.45 else 1.0
            let staffFactor = 1.35 - clamp01 institution.StaffLevel * 0.45
            let congestionFactor = 1.0 + world.Transport.Metrics.AverageCongestion
            let baseMinutes =
                if connected then
                    int (Math.Ceiling(max 3.0 (meters / 420.0)))
                else
                    999

            { Id = policeDispatchId world "street" index
              ReportedActor = reportedActor
              IncidentPlace = Some incidentPlace
              PoliceInstitution = Some institutionId
              Route = if connected then world.Map.RoadSegments |> List.truncate 2 |> List.map _.Id else []
              ExpectedResponseMinutes = if baseMinutes >= 999 then 999 else int (Math.Ceiling(float baseMinutes * fundingFactor * staffFactor * congestionFactor))
              Priority = if reportedActor.IsSome then 2 else 1
              SourceEvent = Some sourceEvent }

    let private neighborhoodForIncident world placeId =
        world.Neighborhoods
        |> Map.toSeq
        |> Seq.tryFind (fun (_, neighborhood) ->
            Set.contains placeId neighborhood.Businesses
            || Set.exists (fun householdId -> world.Households |> Map.tryFind householdId |> Option.exists (fun household -> household.Home = placeId)) neighborhood.Residents)
        |> Option.map fst

    let private consequenceEvents world index actorId incidentPlace description =
        let currentHeat =
            world.Street.Actors
            |> Map.tryFind actorId
            |> Option.map _.Heat
            |> Option.defaultValue NoHeat

        let nextHeat = heatUp currentHeat
        let witnesses = nearbyWitnesses world actorId incidentPlace
        let reportedEventId = eventId world "crime-reported" (index * 100 + 20)
        let dispatch = policeDispatch world (index * 100 + 21) (Some actorId) incidentPlace reportedEventId

        [ for witnessIndex, witness in witnesses |> List.indexed do
              WitnessObservedEvent(eventId world "witness-observed" (index * 100 + witnessIndex), witness, Some actorId, IdentifiedActor, description)
          if not witnesses.IsEmpty then
              CrimeReported(reportedEventId, Some actorId, Some incidentPlace, description)
              PoliceDispatched(eventId world "police-dispatched" (index * 100 + 22), dispatch)
          HeatIncreased(eventId world "heat-increased" (index * 100 + 23), actorId, currentHeat, nextHeat)
          WantedLevelChanged(eventId world "wanted-level" (index * 100 + 24), actorId, currentHeat, nextHeat)
          match neighborhoodForIncident world incidentPlace with
          | Some neighborhoodId ->
              NeighborhoodSafetyChanged(eventId world "neighborhood-safety" (index * 100 + 25), neighborhoodId, -0.02)
              InstitutionalTrustChanged(eventId world "institutional-trust" (index * 100 + 26), dispatch.PoliceInstitution, Some neighborhoodId, -0.005)
          | None -> () ]

    let private resolveStreetCommand world index command =
        let context = contextOfStreetCommand command
        let actorId = context.CommandActor

        let events, affected, warnings =
            match command with
            | MoveActor command ->
                let fromLocation = world.Street.Actors[actorId].Location
                [ ActorMoved(eventId world "actor-moved" index, actorId, fromLocation, ActorAtPlace command.Destination) ],
                [ ActorRef actorId; PlaceEntityRef command.Destination ],
                []
            | EnterVehicle command ->
                [ ActorEnteredVehicle(eventId world "actor-entered-vehicle" index, actorId, command.Vehicle)
                  ActorGainedVehicleControl(eventId world "actor-gained-control" (index * 100 + 1), actorId, command.Vehicle) ],
                [ ActorRef actorId; VehicleRef command.Vehicle ],
                []
            | ExitVehicle _ ->
                let actor = world.Street.Actors[actorId]
                let vehicleId = actor.CurrentVehicle.Value
                let exitLocation =
                    world.Street.Vehicles
                    |> Map.tryFind vehicleId
                    |> Option.map _.Location
                    |> Option.orElseWith (fun () -> actorPlace world actorId |> Option.map ActorAtPlace)
                    |> Option.defaultValue actor.Location

                [ ActorExitedVehicle(eventId world "actor-exited-vehicle" index, actorId, vehicleId, exitLocation)
                  ActorLostVehicleControl(eventId world "actor-lost-control" (index * 100 + 1), actorId, vehicleId) ],
                [ ActorRef actorId; VehicleRef vehicleId ],
                []
            | DriveVehicle command ->
                let vehicle = world.Street.Vehicles[command.Vehicle]
                [ VehicleMoved(eventId world "vehicle-moved" index, command.Vehicle, vehicle.Location, ActorAtPlace command.Destination)
                  ActorMoved(eventId world "driver-moved" (index * 100 + 1), actorId, world.Street.Actors[actorId].Location, ActorInsideVehicle command.Vehicle) ],
                [ ActorRef actorId; VehicleRef command.Vehicle; PlaceEntityRef command.Destination ],
                []
            | AttemptUnauthorizedVehicleAccess command ->
                let succeeded = attemptSucceeds world index actorId (string command.Vehicle) command.Resolution
                let incidentPlace = vehiclePlace world command.Vehicle |> Option.defaultValue (actorPlace world actorId).Value
                let core =
                    [ UnauthorizedVehicleAccessAttempted(eventId world "unauthorized-vehicle-attempt" index, actorId, command.Vehicle)
                      if succeeded then
                          UnauthorizedVehicleAccessSucceeded(eventId world "unauthorized-vehicle-success" (index * 100 + 1), actorId, command.Vehicle)
                          ActorGainedVehicleControl(eventId world "unauthorized-vehicle-control" (index * 100 + 2), actorId, command.Vehicle)
                          ActorEnteredVehicle(eventId world "unauthorized-vehicle-enter" (index * 100 + 3), actorId, command.Vehicle)
                      else
                          UnauthorizedVehicleAccessFailed(eventId world "unauthorized-vehicle-failed" (index * 100 + 1), actorId, command.Vehicle)
                          VehicleAlarmTriggered(eventId world "vehicle-alarm" (index * 100 + 2), command.Vehicle)
                          VehicleDamaged(eventId world "vehicle-damaged" (index * 100 + 3), command.Vehicle, 0.05) ]

                core @ consequenceEvents world index actorId incidentPlace "unauthorized vehicle access attempt",
                [ ActorRef actorId; VehicleRef command.Vehicle ],
                [ "Unauthorized vehicle access resolved abstractly; no real-world technique is modeled." ]
            | EnterBuilding command ->
                [ ActorEnteredBuilding(eventId world "actor-entered-building" index, actorId, command.Building) ],
                [ ActorRef actorId; BuildingRef command.Building ],
                []
            | ExitBuilding command ->
                let actor = world.Street.Actors[actorId]
                let buildingId =
                    match actor.Location with
                    | ActorInsideBuilding buildingId -> buildingId
                    | _ -> command.Context.CommandLocation |> function ActorInsideBuilding buildingId -> buildingId | _ -> world.Street.Buildings |> Map.toSeq |> Seq.head |> fst

                let destination =
                    command.Destination
                    |> Option.map ActorAtPlace
                    |> Option.defaultValue (ActorAtPlace world.Street.Buildings[buildingId].Place)

                [ ActorExitedBuilding(eventId world "actor-exited-building" index, actorId, buildingId, destination) ],
                [ ActorRef actorId; BuildingRef buildingId ],
                []
            | AttemptUnauthorizedEntry command ->
                let succeeded = attemptSucceeds world index actorId (string command.Building) command.Resolution
                let building = world.Street.Buildings[command.Building]
                let core =
                    [ UnauthorizedEntryAttempted(eventId world "unauthorized-entry-attempt" index, actorId, command.Building)
                      if succeeded then
                          UnauthorizedEntrySucceeded(eventId world "unauthorized-entry-success" (index * 100 + 1), actorId, command.Building)
                          ActorEnteredBuilding(eventId world "unauthorized-entry-entered" (index * 100 + 2), actorId, command.Building)
                      else
                          UnauthorizedEntryFailed(eventId world "unauthorized-entry-failed" (index * 100 + 1), actorId, command.Building)
                          TrespassReported(eventId world "trespass-reported" (index * 100 + 2), Some actorId, Some command.Building) ]

                core @ consequenceEvents world index actorId building.Place "unauthorized building entry attempt",
                [ ActorRef actorId; BuildingRef command.Building ],
                [ "Unauthorized entry resolved abstractly; no bypass method is modeled." ]
            | PurchaseItem command ->
                [ ItemPurchased(eventId world "item-purchased" index, actorId, command.Seller, command.Item, command.Item.Price) ],
                [ ActorRef actorId; PlaceEntityRef command.Seller; ItemRef command.Item.Id ],
                []
            | AttemptTakeItemWithoutPayment command ->
                let succeeded = attemptSucceeds world index actorId (string command.Item.Id) command.Resolution
                let core =
                    [ if succeeded then
                          ItemTakenWithoutPayment(eventId world "item-taken-without-payment" index, actorId, command.FromPlace, command.Item)
                      else
                          BusinessInterrupted(eventId world "business-interrupted" index, command.FromPlace, 0.05)
                      TheftReported(eventId world "theft-reported" (index * 100 + 1), Some actorId, Some command.FromPlace) ]

                core @ consequenceEvents world index actorId command.FromPlace "taking item without payment",
                [ ActorRef actorId; PlaceEntityRef command.FromPlace; ItemRef command.Item.Id ],
                [ "Taking without payment is separate from purchase and resolves into consequences." ]
            | InteractWithPerson command ->
                [ PersonInteractionOccurred(eventId world "person-interaction" index, actorId, command.Target) ],
                [ ActorRef actorId; ActorRef command.Target ],
                []
            | StartConflict command ->
                [ ConflictStarted(eventId world "conflict-started" index, actorId, command.Target) ],
                [ ActorRef actorId; ActorRef command.Target ],
                [ "Conflict is abstract; no weapon or combat mechanics are modeled." ]
            | DamageProperty command ->
                let place = actorPlace world actorId
                let core = [ PropertyDamaged(eventId world "property-damaged" index, actorId, command.Target, command.Severity) ]
                let consequence =
                    place
                    |> Option.map (fun place -> consequenceEvents world index actorId place "property damage")
                    |> Option.defaultValue []

                core @ consequence, [ ActorRef actorId; command.Target ], []
            | FleeScene command ->
                let destination =
                    command.Destination
                    |> Option.orElseWith (fun () ->
                        actorPlace world actorId
                        |> Option.bind (fun place -> world.Street.PlaceConnections |> Map.tryFind place |> Option.bind (Seq.tryHead)))

                match destination with
                | Some place ->
                    [ ActorMoved(eventId world "actor-fled" index, actorId, world.Street.Actors[actorId].Location, ActorAtPlace place)
                      HeatIncreased(eventId world "flee-heat" (index * 100 + 1), actorId, world.Street.Actors[actorId].Heat, heatUp world.Street.Actors[actorId].Heat) ],
                    [ ActorRef actorId; PlaceEntityRef place ],
                    []
                | None -> [], [ ActorRef actorId ], [ "No escape destination was available." ]
            | ReportCrime command ->
                let incidentPlace = command.IncidentPlace |> Option.orElse (actorPlace world actorId)
                match incidentPlace with
                | Some place ->
                    let reported = eventId world "reported-crime" index
                    let dispatch = policeDispatch world index command.ReportedActor place reported
                    [ CrimeReported(reported, command.ReportedActor, Some place, "actor reported incident")
                      PoliceDispatched(eventId world "reported-police-dispatch" (index * 100 + 1), dispatch) ],
                    [ ActorRef actorId; PlaceEntityRef place ],
                    []
                | None -> [], [ ActorRef actorId ], [ "No incident place was available." ]
            | CallEmergencyService command ->
                [ EmergencyServiceCalled(eventId world "emergency-called" index, Some actorId, command.IncidentPlace) ],
                [ ActorRef actorId ],
                []
            | Trespass command ->
                let building = world.Street.Buildings[command.Building]
                [ TrespassReported(eventId world "trespass" index, Some actorId, Some command.Building) ]
                @ consequenceEvents world index actorId building.Place "trespass",
                [ ActorRef actorId; BuildingRef command.Building ],
                []
            | SurrenderToPolice _ ->
                let institution =
                    world.Institutions
                    |> Map.toSeq
                    |> Seq.tryFind (fun (_, institution) -> institution.Kind = PoliceInstitution)
                    |> Option.map fst

                [ ActorDetained(eventId world "actor-surrendered" index, actorId, institution) ],
                [ ActorRef actorId ],
                []
            | UseItem command ->
                [ ObjectUsed(eventId world "object-used" index, actorId, command.Item) ],
                [ ActorRef actorId; ItemRef command.Item ],
                []
            | InteractWithObject command ->
                [ ObjectUsed(eventId world "object-interaction" index, actorId, command.Target) ],
                [ ActorRef actorId; ItemRef command.Target ],
                []
            | DebugTeleportActor command ->
                [ ActorMoved(eventId world "debug-teleport" index, actorId, world.Street.Actors[actorId].Location, command.Destination) ],
                [ ActorRef actorId ],
                [ "Debug teleport bypasses route constraints by explicit debug command." ]

        events, affected, warnings

    let resolveCommand (world: World) index (validated: ValidatedCommand) : ResolvedCommand =
        let command = validated.Command
        let source = sourceOf command
        let cost = commandCost command

        let events, affected, warnings =
            match command with
            | StreetCommand command ->
                resolveStreetCommand world index command
            | BuildBuilding command ->
                let buildingId = buildingIdForParcel command.TargetParcel
                let building = buildingFromCommand command
                let jobs =
                    if building.Jobs <= 0 then
                        []
                    else
                        [ 1..building.Jobs ] |> List.map (jobId world "build-building" index)

                [ BuildingConstructed(eventId world "building-constructed" index, source, buildingId, command.TargetParcel, building, command.ConstructionCost)
                  if not jobs.IsEmpty then
                      JobsCreated(eventId world "jobs-created" index, source, jobs) ],
                [ ParcelRef command.TargetParcel; BuildingRef buildingId ],
                []
            | DestroyBuilding command ->
                match parcelForBuilding command.BuildingId world with
                | Some(parcelId, parcel, building) ->
                    let displaced = displacedHouseholdsForBuilding parcel building world

                    [ BuildingDestroyed(eventId world "building-destroyed" index, source, command.BuildingId, parcelId, command.Reason, command.DemolitionCost)
                      if not displaced.IsEmpty then
                          HouseholdsDisplaced(eventId world "households-displaced" index, source, Some command.BuildingId, displaced) ],
                    BuildingRef command.BuildingId :: (displaced |> List.map HouseholdRef),
                    if displaced.IsEmpty then [] else [ "Destroyed occupied housing; households were displaced." ]
                | None -> [], [ BuildingRef command.BuildingId ], []
            | RezoneParcels command ->
                command.ParcelIds
                |> List.mapi (fun parcelIndex parcelId ->
                    ParcelRezoned(eventId world "parcel-rezoned" (index * 1000 + parcelIndex), source, parcelId, command.FromZone, command.ToZone)),
                command.ParcelIds |> List.map ParcelRef,
                [ "Zoning changes legal possibility only; no building is created by this command." ]
            | ZoneParcels command ->
                command.ParcelIds
                |> List.mapi (fun parcelIndex parcelId ->
                    ParcelZoned(eventId world "parcel-zoned" (index * 1000 + parcelIndex), source, parcelId, command.ZoneType, command.AllowedDensity)),
                command.ParcelIds |> List.map ParcelRef,
                []
            | BuildRoad command ->
                let segment, lanes = roadFromCommand world command index

                [ RoadBuilt(eventId world "road-built" index, source, segment, lanes, command.Cost) ],
                RoadSegmentRef segment.Id :: (lanes |> List.map (fun lane -> LaneRef lane.Id)),
                [ "Transport route and travel-time caches must be recalculated." ]
            | DestroyRoad command ->
                [ RoadDestroyed(eventId world "road-destroyed" index, source, command.RoadSegmentId, command.Reason) ],
                [ RoadSegmentRef command.RoadSegmentId ],
                [ if not (roadNetworkConnectedAfterRemoving command.RoadSegmentId world) then
                      "Road removal disconnects part of the road network; rerouting is required." ]
            | _ -> [], [], [ "Command model exists but this command has no deep resolver yet." ]

        ({ Validated = validated
           ActualCost = cost
           AffectedEntities = affected
           Events = events
           Warnings = warnings }
         : ResolvedCommand)

    let resolveCommands (world: World) (validatedCommands: ValidatedCommand list) : ResolvedCommand list =
        validatedCommands
        |> List.mapi (resolveCommand world)

    let executeCommandBatch mode world commands =
        let validations = commands |> List.map (fun command -> validateCommand mode world command)

        let validCommands =
            validations
            |> List.choose (function
                | Valid command -> Some command
                | Invalid _ -> None)

        let rejections =
            validations
            |> List.collect (function
                | Valid _ -> []
                | Invalid rejections -> rejections)

        let resolved = resolveCommands world validCommands
        let events = resolved |> List.collect _.Events
        let world = SimulationPipeline.applyEvents events world

        { World = world
          ResolvedCommands = resolved
          Rejections = rejections
          Events = events }

    let submitPlayerCommand playerId command world =
        let actor =
            world.Street.Actors
            |> Map.toSeq
            |> Seq.tryFind (fun (_, actor) -> actor.Control = PlayerControlled playerId)
            |> Option.map fst

        let mode =
            actor
            |> Option.map StreetLevelActorMode
            |> Option.defaultValue CityManagerMode

        executeCommandBatch mode world [ command ]

    let private interactionIdFor world actorId label =
        InteractionId(stableGuid [ string world.Meta.Seed; string world.Meta.Tick; string actorId; label ])

    let private positionOfLocation world location =
        placeOfActorLocation world location
        |> Option.bind (fun placeId -> world.Map.Places |> Map.tryFind placeId)
        |> Option.map _.Position

    let private actorDto (world: World) (playerActor: ActorId) (actor: Actor) : ActorDto =
        { Id = actor.Id
          Name = actor.Name
          Position = positionOfLocation world actor.Location
          CurrentActivity = string actor.CurrentActivity
          RelationshipToPlayer = if actor.Id = playerActor then Some "self" else actor.Relationships |> Map.tryFind playerActor |> Option.map string
          Status = string actor.Health
          Heat = if actor.Heat = NoHeat then None else Some(string actor.Heat) }

    let private vehicleDto (world: World) (playerActor: ActorId) (vehicle: StreetVehicle) : VehicleDto =
        { Id = vehicle.Id
          Name = vehicle.Name
          Position = positionOfLocation world vehicle.Location
          Access = string vehicle.Access
          ControlledByPlayer = vehicle.Controller = Some playerActor }

    let private nearbyPlaces (world: World) center radius : PlaceDto list =
        match placeOfActorLocation world center with
        | None -> []
        | Some centerPlace ->
            world.Map.Places
            |> Map.toSeq
            |> Seq.choose (fun (placeId, place) ->
                if distanceBetweenPlaces world centerPlace placeId <= radius then
                    Some
                        { Id = placeId
                          Name = place.Name
                          Kind = string place.Kind
                          Position = Some place.Position }
                else
                    None)
            |> Seq.sortBy _.Name
            |> Seq.toList

    let private availableInteractions (world: World) (actor: Actor) : InteractionPromptDto list =
        let currentPlace = placeOfActorLocation world actor.Location

        match currentPlace with
        | None -> []
        | Some place ->
            let context label =
                { CommandSource = PlayerCommand None
                  CommandActor = actor.Id
                  CommandLocation = actor.Location
                  CommandTick = TickId world.Meta.Tick
                  IntendedAction = label
                  StreetExpectedConsequences = [] }

            let movePrompts =
                world.Street.PlaceConnections
                |> Map.tryFind place
                |> Option.defaultValue Set.empty
                |> Seq.choose (fun destination ->
                    world.Map.Places
                    |> Map.tryFind destination
                    |> Option.map (fun destinationPlace ->
                        let command = StreetCommand(MoveActor { Context = context $"Move to {destinationPlace.Name}"; Destination = destination })
                        let prompt : InteractionPromptDto =
                            { Id = interactionIdFor world actor.Id destinationPlace.Name
                              Label = destinationPlace.Name
                              Command = command
                              IsEnabled = true
                              DisabledReason = None
                              Warnings = [] }

                        prompt))
                |> Seq.toList

            let vehiclePrompts =
                world.Street.Vehicles
                |> Map.toSeq
                |> Seq.choose (fun (_, vehicle) ->
                    if isWithinDirectRange world actor.Location vehicle.Location then
                        let prompt : InteractionPromptDto =
                            { Id = interactionIdFor world actor.Id vehicle.Name
                              Label = vehicle.Name
                              Command =
                                  StreetCommand(
                                      AttemptUnauthorizedVehicleAccess
                                          { Context = context "Attempt vehicle access"
                                            Vehicle = vehicle.Id
                                            Resolution = ResolveByRiskModel })
                              IsEnabled = true
                              DisabledReason = None
                              Warnings = [ "UnauthorizedAction"; "WitnessRisk" ] }

                        Some prompt
                    else
                        None)
                |> Seq.toList

            movePrompts @ vehiclePrompts

    let queryStreetView playerId query world =
        let playerActor =
            world.Street.Actors
            |> Map.toSeq
            |> Seq.tryFind (fun (_, actor) -> actor.Control = PlayerControlled playerId)
            |> Option.orElseWith (fun () -> world.Street.Actors |> Map.toSeq |> Seq.tryHead)
            |> Option.defaultWith (fun () -> invalidArg "playerId" "No actor is available for street view.")

        let playerActorId, player = playerActor
        let center = query.Center |> Option.defaultValue player.Location
        let radius = if query.RadiusMeters <= 0.0 then 150.0 else query.RadiusMeters

        let near location =
            match placeOfActorLocation world center, placeOfActorLocation world location with
            | Some a, Some b -> a = b || distanceBetweenPlaces world a b <= radius
            | _ -> false

        { Player = actorDto world playerActorId player
          NearbyActors =
            world.Street.Actors
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun actor -> actor.Id <> playerActorId && near actor.Location)
            |> Seq.map (actorDto world playerActorId)
            |> Seq.toList
          NearbyVehicles =
            world.Street.Vehicles
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun vehicle -> near vehicle.Location)
            |> Seq.map (vehicleDto world playerActorId)
            |> Seq.toList
          NearbyPlaces = nearbyPlaces world center radius
          NearbyEvents =
            world.Street.RecentEventIds
            |> List.map (fun eventId ->
                { Id = eventId
                  Kind = "StreetEvent"
                  Description = string eventId })
          AvailableInteractions = availableInteractions world player
          Heat = { Level = string player.Heat }
          Time = { Day = world.Day; MinuteOfDay = world.MinuteOfDay } }
