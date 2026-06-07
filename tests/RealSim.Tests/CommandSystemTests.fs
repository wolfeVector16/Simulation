namespace RealSim.Tests

open System
open Xunit
open Simulation
open Simulation.Domain

module CommandSystemTests =
    let private defaultUtilityDemand =
        { Power = 1.0
          Water = 1.0
          Sewage = 1.0
          Garbage = 1.0 }

    let private defaultAccessibility =
        { WalkAccess = 0.7
          BikeAccess = 0.5
          TransitAccess = 0.4
          CarAccess = 0.8
          FreightAccess = 0.2
          EmergencyAccess = 0.8 }

    let private playerSource = PlayerCommand None

    let private worldWithVacantResidentialParcel () =
        let world = TestWorld.create ()
        let parcelId, parcel = world.City.Parcels |> Map.toSeq |> Seq.head
        let parcel = { parcel with Zone = ResidentialZone; Building = None; RoadConnected = true }

        { world with
            City = { world.City with Parcels = Map.add parcelId parcel world.City.Parcels } },
        parcelId

    let private occupiedHousingParcel world =
        world.City.Parcels
        |> Map.toSeq
        |> Seq.find (fun (_, parcel) ->
            parcel.Building
            |> Option.exists (fun building -> building.Use = Housing && building.Occupants > 0))

    let private buildBuilding parcelId =
        BuildBuilding
            { Source = playerSource
              TargetParcel = parcelId
              BuildingUse = Housing
              IntendedOwner = PublicOwner
              ConstructionCost = 1000m
              EstimatedConstructionTime = 120
              FundingSource = CityTreasury
              RequiredZoning = ResidentialZone
              ExpectedCapacity = 6
              OptionalInstitutionKind = None
              ParkingSupply = 2
              UtilityDemand = defaultUtilityDemand
              AccessibilityProfile = defaultAccessibility }

    let private destroyBuilding source buildingId =
        DestroyBuilding
            { Source = source
              BuildingId = buildingId
              Reason = PlayerDemolition
              DemolitionCost = 500m
              DisplacementPolicy = None
              PreserveHistoricalMemory = true
              DebrisCleanupRequired = true }

    let private rezoneParcel parcelId fromZone toZone (world: World) =
        RezoneParcels
            { Source = playerSource
              ParcelIds = [ parcelId ]
              FromZone = fromZone
              ToZone = toZone
              PoliticalCost = 0.2
              LegalRisk = 0.1
              DisplacementRisk = 0.0
              EffectiveDate = ({ Day = world.Day; MinuteOfDay = world.MinuteOfDay }: SimTime) }

    let private laneSpec direction =
        { LaneType = General
          Direction = direction
          AllowedModes = [ PrivateCar; Bus; EmergencyVehicle; DeliveryVehicle ] |> Set.ofList
          PermittedMovements = [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList }

    let private buildRoad world =
        let nodes = world.Map.RoadNodes |> Map.toSeq |> Seq.map fst |> Seq.toList

        BuildRoad
            { Source = playerSource
              RoadClass = Collector
              FromNode = nodes[0]
              ToNode = nodes[1]
              Lanes = [ laneSpec Forward; laneSpec Reverse ]
              Sidewalks = true
              BikeFacilities = PaintedBikeLane
              TransitPriority = false
              SpeedLimit = 35.0
              Cost = 1000m
              ConstructionTime = 90
              AffectedParcels = []
              AffectedNeighborhoods = [] }

    let private execute mode world command =
        CommandSystem.executeCommandBatch mode world [ command ]

    [<Fact>]
    let ``BuildBuildingCreatesBuildingEvent`` () =
        let world, parcelId = worldWithVacantResidentialParcel ()

        let result = buildBuilding parcelId |> execute SandboxGodMode world

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function BuildingConstructed _ -> true | _ -> false)
        Assert.True(result.World.City.Parcels[parcelId].Building.IsSome)

    [<Fact>]
    let ``DestroyBuildingDisplacesHouseholds`` () =
        let world = TestWorld.create ()
        let parcelId, parcel = occupiedHousingParcel world
        let buildingId = BuildingId parcelId
        let beforeStableHouseholds = world.Households

        let result = destroyBuilding playerSource buildingId |> execute SandboxGodMode world

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function BuildingDestroyed _ -> true | _ -> false)
        Assert.Contains(result.Events, function HouseholdsDisplaced _ -> true | _ -> false)
        Assert.True(result.World.City.Parcels[parcelId].Building.IsNone)

        let displaced =
            result.Events
            |> List.choose (function HouseholdsDisplaced(_, _, _, households) -> Some households | _ -> None)
            |> List.concat

        Assert.NotEmpty(displaced)

        displaced
        |> List.iter (fun householdId ->
            Assert.Equal(Shelter, result.World.Households[householdId].HousingStatus)
            Assert.True(result.World.Households[householdId].Stability < beforeStableHouseholds[householdId].Stability))

        Assert.True(result.World.Memories.Count > world.Memories.Count)

    [<Fact>]
    let ``DestroyBuildingWithoutAuthorityIsRejected`` () =
        let world = TestWorld.create ()
        let parcelId, _ = occupiedHousingParcel world
        let command = destroyBuilding playerSource (BuildingId parcelId)

        let result = execute MayorMode world command

        Assert.Contains(result.Rejections, function OccupiedBuildingRequiresDisplacementPlan _ -> true | _ -> false)
        Assert.Empty(result.Events)
        Assert.True(world.City.Parcels = result.World.City.Parcels)

    [<Fact>]
    let ``SandboxCanDestroyBuildingButStillEmitsConsequences`` () =
        let world = TestWorld.create ()
        let parcelId, _ = occupiedHousingParcel world

        let result = destroyBuilding playerSource (BuildingId parcelId) |> execute SandboxGodMode world

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function BuildingDestroyed _ -> true | _ -> false)
        Assert.Contains(result.Events, function HouseholdsDisplaced _ -> true | _ -> false)

    [<Fact>]
    let ``RezoneParcelChangesAllowedUseButDoesNotCreateBuilding`` () =
        let world, parcelId = worldWithVacantResidentialParcel ()
        let beforeBuilding = world.City.Parcels[parcelId].Building

        let result = rezoneParcel parcelId ResidentialZone MixedUseZone world |> execute PlannerMode world

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function ParcelRezoned _ -> true | _ -> false)
        Assert.Equal(MixedUseZone, result.World.City.Parcels[parcelId].Zone)
        Assert.Equal(beforeBuilding, result.World.City.Parcels[parcelId].Building)

    [<Fact>]
    let ``BuildRoadUpdatesTransportNetwork`` () =
        let world = TestWorld.create ()
        let beforeRoads = world.Map.RoadSegments.Length
        let beforeLanes = world.Transport.Lanes.Count
        let beforeCacheVersion = world.Runtime.CacheVersion

        let result = buildRoad world |> execute PlannerMode world

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function RoadBuilt _ -> true | _ -> false)
        Assert.Equal(beforeRoads + 1, result.World.Map.RoadSegments.Length)
        Assert.Equal(beforeLanes + 2, result.World.Transport.Lanes.Count)
        Assert.True(result.World.Runtime.CacheVersion > beforeCacheVersion)
        Assert.Empty(result.World.Runtime.RouteCache)
        Assert.Empty(result.World.Runtime.TravelTimeCache)

    [<Fact>]
    let ``DestroyRoadCanTriggerConnectivityWarning`` () =
        let world = TestWorld.create ()

        let disconnectingCommand =
            world.Map.RoadSegments
            |> List.map (fun segment ->
                DestroyRoad
                    { Source = playerSource
                      RoadSegmentId = segment.Id
                      Reason = PlayerRemoval
                      CleanupCost = 100m
                      RerouteRequired = false })
            |> List.tryFind (fun command ->
                match CommandSystem.validateCommand MayorMode world command with
                | Invalid rejections -> rejections |> List.exists (function WouldDisconnectRoadNetwork _ -> true | _ -> false)
                | Valid _ -> false)

        Assert.True(disconnectingCommand.IsSome, "Expected at least one road command to trigger a connectivity warning/rejection.")

    [<Fact>]
    let ``DisasterUsesSameCommandPipeline`` () =
        let world = TestWorld.create ()
        let parcelId, _ = occupiedHousingParcel world
        let disasterId = DisasterId(Guid.Parse("aaaaaaaa-0000-0000-0000-000000000001"))
        let command = destroyBuilding (DisasterCommand disasterId) (BuildingId parcelId)

        let result = execute ObserverMode world command

        Assert.Empty(result.Rejections)
        Assert.Contains(result.Events, function BuildingDestroyed(_, DisasterCommand id, _, _, _, _) when id = disasterId -> true | _ -> false)
        Assert.True(result.World.City.Parcels[parcelId].Building.IsNone)

    [<Fact>]
    let ``InvalidCommandDoesNotMutateWorld`` () =
        let world = TestWorld.create ()
        let missingParcel = ParcelId(Guid.Parse("bbbbbbbb-0000-0000-0000-000000000001"))

        let result = buildBuilding missingParcel |> execute SandboxGodMode world

        Assert.Contains(result.Rejections, function EntityNotFound(ParcelRef parcelId) when parcelId = missingParcel -> true | _ -> false)
        Assert.Empty(result.Events)
        Assert.Equal(world.City, result.World.City)
        Assert.Equal(world.Map, result.World.Map)

    [<Fact>]
    let ``PreviewCommandDoesNotMutateWorld`` () =
        let world = TestWorld.create ()
        let parcelId, _ = occupiedHousingParcel world
        let command = destroyBuilding playerSource (BuildingId parcelId)
        let before = world

        let preview = CommandSystem.previewCommandForWorld MayorMode world command

        Assert.True(preview.ExpectedDisplacementRisk > 0.0)
        Assert.NotEmpty(preview.Warnings)
        Assert.Equal(before, world)

    [<Fact>]
    let ``CommandDeterminism`` () =
        let world1, parcelId = worldWithVacantResidentialParcel ()
        let world2, _ = worldWithVacantResidentialParcel ()
        let input = { Tick = TickId world1.Meta.Tick; Commands = [ buildBuilding parcelId ] }

        let next1, result1 = Engine.advanceTickWithAuthority SandboxGodMode 15 input world1
        let next2, result2 = Engine.advanceTickWithAuthority SandboxGodMode 15 input world2

        Assert.True((result1.Events.Events |> Array.map string) = (result2.Events.Events |> Array.map string))
        Assert.True((next1.Meta.EventLog |> List.map string) = (next2.Meta.EventLog |> List.map string))
        Assert.True(next1.City.Parcels = next2.City.Parcels)
        Invariants.checkWorld next1 |> ignore
