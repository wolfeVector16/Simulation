namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain

module StreetSimulationTests =
    let private playerActor (world: World) =
        world.Street.Actors
        |> Map.toSeq
        |> Seq.find (fun (_, actor) -> match actor.Control with PlayerControlled _ -> true | _ -> false)

    let private playerId (actor: Actor) =
        match actor.Control with
        | PlayerControlled playerId -> playerId
        | _ -> failwith "Expected player actor."

    let private actorByName name (world: World) =
        world.Street.Actors |> Map.toSeq |> Seq.find (fun (_, actor) -> actor.Name = name)

    let private placeByName name (world: World) =
        world.Map.Places |> Map.toSeq |> Seq.find (fun (_, place) -> place.Name = name) |> fst

    let private buildingByName name (world: World) =
        world.Street.Buildings |> Map.toSeq |> Seq.find (fun (_, building) -> building.Name = name) |> fst

    let private vehicle (world: World) =
        world.Street.Vehicles |> Map.toSeq |> Seq.head |> fst

    let private context (world: World) actorId label =
        let actor = world.Street.Actors[actorId]
        { CommandSource = PlayerCommand None
          CommandActor = actorId
          CommandLocation = actor.Location
          CommandTick = TickId world.Meta.Tick
          IntendedAction = label
          StreetExpectedConsequences = [] }

    let private item price =
        { Id = ItemId(Guid.Parse("cccccccc-0000-0000-0000-000000000001"))
          Name = "test item"
          Category = PurchasedGood
          Good = Some Groceries
          Price = price
          OwnerLabel = Some "test shop" }

    let private executeStreet world command =
        let actorId, _ = playerActor world
        CommandSystem.executeCommandBatch (StreetLevelActorMode actorId) world [ StreetCommand command ]

    let private moveTo name world =
        let actorId, _ = playerActor world
        executeStreet world (MoveActor { Context = context world actorId "move"; Destination = placeByName name world })

    let private unauthorizedVehicle resolution world =
        let actorId, _ = playerActor world
        AttemptUnauthorizedVehicleAccess
            { Context = context world actorId "attempt vehicle access"
              Vehicle = vehicle world
              Resolution = resolution }

    [<Fact>]
    let ``StreetCommandUsesSamePipeline`` () =
        let world = TestWorld.create ()
        let result = moveTo "Canal Apartments" world

        Assert.Empty(result.Rejections)
        Assert.NotEmpty(result.ResolvedCommands)
        Assert.Contains(result.Events, function ActorMoved _ -> true | _ -> false)

    [<Fact>]
    let ``InvalidStreetCommandDoesNotMutateWorld`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let missing = ActorId(Guid.Parse("dddddddd-0000-0000-0000-000000000001"))

        let result =
            executeStreet world (InteractWithPerson { Context = context world actorId "bad interaction"; Target = missing })

        Assert.NotEmpty(result.Rejections)
        Assert.Empty(result.Events)
        Assert.Equal(world.Street, result.World.Street)

    [<Fact>]
    let ``UnauthorizedIsNotAutomaticallyRejected`` () =
        let world = TestWorld.create ()
        let result = executeStreet world (unauthorizedVehicle ForceFailure world)

        Assert.Empty(result.Rejections)
        Assert.Contains(result.ResolvedCommands.Head.Validated.CommandWarnings, ((=) UnauthorizedAction))

    [<Fact>]
    let ``ActorCanMoveBetweenConnectedPlaces`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let destination = placeByName "Canal Apartments" world
        let result = moveTo "Canal Apartments" world

        Assert.Empty(result.Rejections)
        Assert.Equal(ActorAtPlace destination, result.World.Street.Actors[actorId].Location)

    [<Fact>]
    let ``ActorCannotMoveToUnreachablePlace`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world

        let result =
            executeStreet world (MoveActor { Context = context world actorId "move"; Destination = placeByName "Regional Galleria" world })

        Assert.NotEmpty(result.Rejections)
        Assert.Empty(result.Events)

    [<Fact>]
    let ``UnauthorizedVehicleAccessIsAttemptable`` () =
        let world = TestWorld.create ()
        let result = executeStreet world (unauthorizedVehicle ResolveByRiskModel world)

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function UnauthorizedVehicleAccessAttempted _ -> true | _ -> false)

    [<Fact>]
    let ``UnauthorizedVehicleAccessCanSucceedWithConsequences`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let vehicleId = vehicle world
        let result = executeStreet world (unauthorizedVehicle ForceSuccess world)

        Assert.Equal(Some actorId, result.World.Street.Vehicles[vehicleId].Controller)
        Assert.Contains(result.Events, function UnauthorizedVehicleAccessSucceeded _ -> true | _ -> false)
        Assert.Contains(result.Events, function HeatIncreased _ -> true | _ -> false)

    [<Fact>]
    let ``UnauthorizedVehicleAccessCanFailWithConsequences`` () =
        let world = TestWorld.create ()
        let vehicleId = vehicle world
        let result = executeStreet world (unauthorizedVehicle ForceFailure world)

        Assert.Equal(None, result.World.Street.Vehicles[vehicleId].Controller)
        Assert.Contains(result.Events, function UnauthorizedVehicleAccessFailed _ -> true | _ -> false)
        Assert.Contains(result.Events, function VehicleAlarmTriggered _ -> true | _ -> false)

    [<Fact>]
    let ``CannotDriveWithoutVehicleControl`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world

        let result =
            executeStreet world (
                DriveVehicle
                    { Context = context world actorId "drive"
                      Vehicle = vehicle world
                      Destination = placeByName "Main Street Goods" world })

        Assert.NotEmpty(result.Rejections)

    [<Fact>]
    let ``RestrictedBuildingEntryIsAttemptable`` () =
        let moved = (moveTo "Canal Apartments" (TestWorld.create ())).World
        let actorId, _ = playerActor moved

        let result =
            executeStreet moved (
                AttemptUnauthorizedEntry
                    { Context = context moved actorId "entry"
                      Building = buildingByName "Canal Apartments" moved
                      Resolution = ForceSuccess })

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function UnauthorizedEntryAttempted _ -> true | _ -> false)

    [<Fact>]
    let ``OutOfRangeInteractionIsRejected`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let target, _ = actorByName "Opportunistic NPC" world

        let result = executeStreet world (InteractWithPerson { Context = context world actorId "talk"; Target = target })

        Assert.NotEmpty(result.Rejections)

    [<Fact>]
    let ``PurchaseWithoutFundsFailsButTakingIsSeparateCommand`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let expensive = item 5000m
        let home = placeByName "Rowhouse 12" world

        let purchase =
            executeStreet world (PurchaseItem { Context = context world actorId "purchase"; Seller = home; Item = expensive })

        let taking =
            executeStreet world (
                AttemptTakeItemWithoutPayment
                    { Context = context world actorId "take"
                      FromPlace = home
                      Item = expensive
                      Resolution = ForceSuccess })

        Assert.NotEmpty(purchase.Rejections)
        Assert.Empty(taking.Rejections)

    [<Fact>]
    let ``IllegalActionDoesNotBypassInvariants`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let missingPlace = PlaceId(Guid.Parse("eeeeeeee-0000-0000-0000-000000000001"))

        let result =
            executeStreet world (
                AttemptTakeItemWithoutPayment
                    { Context = context world actorId "bad take"
                      FromPlace = missingPlace
                      Item = item 1m
                      Resolution = ForceSuccess })

        Assert.NotEmpty(result.Rejections)
        Assert.Empty(result.Events)

    [<Fact>]
    let ``WitnessCanReportUnauthorizedAction`` () =
        let world = TestWorld.create ()
        let result = executeStreet world (unauthorizedVehicle ForceSuccess world)

        Assert.Contains(result.Events, function WitnessObservedEvent _ -> true | _ -> false)
        Assert.Contains(result.Events, function CrimeReported _ -> true | _ -> false)

    [<Fact>]
    let ``PoliceDispatchUsesTransportLayer`` () =
        let world = TestWorld.create ()
        let result = executeStreet world (unauthorizedVehicle ForceSuccess world)

        let dispatch =
            result.Events
            |> List.choose (function PoliceDispatched(_, dispatch) -> Some dispatch | _ -> None)
            |> List.head

        Assert.True(dispatch.ExpectedResponseMinutes < 999)
        Assert.NotEmpty(dispatch.Route)

    [<Fact>]
    let ``HeatIncreasesAfterReportedCrime`` () =
        let world = TestWorld.create ()
        let actorId, _ = playerActor world
        let result = executeStreet world (unauthorizedVehicle ForceSuccess world)

        Assert.NotEqual(NoHeat, result.World.Street.Actors[actorId].Heat)

    [<Fact>]
    let ``HeatDecaysDeterministically`` () =
        let world = TestWorld.create ()
        let hot = (executeStreet world (unauthorizedVehicle ForceSuccess world)).World
        let cooled1 = hot |> Engine.tick 15 |> Engine.tick 15
        let cooled2 = hot |> Engine.tick 15 |> Engine.tick 15
        let actorId, _ = playerActor hot

        Assert.Equal(cooled1.Street.Actors[actorId].Heat, cooled2.Street.Actors[actorId].Heat)
        Assert.Equal(NoHeat, cooled1.Street.Actors[actorId].Heat)

    [<Fact>]
    let ``StreetActionAffectsNeighborhoodSafety`` () =
        let world = TestWorld.create ()
        let neighborhoodId = world.Neighborhoods |> Map.toSeq |> Seq.head |> fst
        let before = world.Neighborhoods[neighborhoodId].Safety
        let result = executeStreet world (unauthorizedVehicle ForceSuccess world)

        Assert.True(result.World.Neighborhoods[neighborhoodId].Safety < before)

    [<Fact>]
    let ``CityPolicyAffectsStreetOutcome`` () =
        let tune funding (world: World) : World =
            { world with
                Institutions =
                    world.Institutions
                    |> Map.map (fun _ (institution: Institution) -> if institution.Kind = PoliceInstitution then { institution with Funding = funding } else institution) }

        let responseMinutes world =
            executeStreet world (unauthorizedVehicle ForceSuccess world)
            |> fun result ->
                result.Events
                |> List.choose (function PoliceDispatched(_, dispatch) -> Some dispatch.ExpectedResponseMinutes | _ -> None)
                |> List.head

        let low = TestWorld.create () |> tune 100m |> responseMinutes
        let high = TestWorld.create () |> tune 1000m |> responseMinutes

        Assert.True(high < low)

    [<Fact>]
    let ``StreetViewSnapshotIncludesNearbyEntities`` () =
        let world = TestWorld.create ()
        let _, actor = playerActor world
        let snapshot = CommandSystem.queryStreetView (playerId actor) { Center = None; RadiusMeters = 500.0 } world

        Assert.Equal(actor.Id, snapshot.Player.Id)
        Assert.NotEmpty(snapshot.NearbyActors)
        Assert.NotEmpty(snapshot.NearbyVehicles)
        Assert.NotEmpty(snapshot.NearbyPlaces)
        Assert.NotEmpty(snapshot.AvailableInteractions)

    [<Fact>]
    let ``SameSeedSameStreetEvents`` () =
        let world1 = TestWorld.create ()
        let world2 = TestWorld.create ()
        let result1 = executeStreet world1 (unauthorizedVehicle ResolveByRiskModel world1)
        let result2 = executeStreet world2 (unauthorizedVehicle ResolveByRiskModel world2)

        Assert.Equal<string list>(result1.Events |> List.map string, result2.Events |> List.map string)
