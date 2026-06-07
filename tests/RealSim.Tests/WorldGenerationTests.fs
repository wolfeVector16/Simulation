namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain
open RealSim.Scenarios

module WorldGenerationTests =
    [<Fact>]
    let ``JuniperScenarioBuildsAndPassesValidation`` () =
        let world = Juniper.createSampleWorld ()

        let failures =
            world.GenerationReport.Findings
            |> List.filter (fun finding -> finding.Status = ValidationStatus.Failed)

        Invariants.checkWorld world |> ignore
        Assert.Equal("Juniper Valley micro-region", world.Region.Name)
        Assert.Empty(failures)

    [<Fact>]
    let ``WorldIndexesCalculatesInstitutionCapacityUsage`` () =
        let world = Juniper.createSampleWorld ()
        let usages = WorldIndexes.usedCapacityByInstitution world

        Assert.Contains(world.Institutions |> Map.toSeq, fun (institutionId, _) ->
            usages |> Map.containsKey institutionId)

        world.Institutions
        |> Map.iter (fun institutionId institution ->
            Assert.True(WorldIndexes.usedCapacity institutionId world <= institution.Capacity, $"{institution.Name} exceeds derived capacity."))

    [<Fact>]
    let ``InstitutionFailureModesAreTyped`` () =
        let world = Juniper.createSampleWorld ()

        let modes =
            world.Institutions
            |> Map.toSeq
            |> Seq.collect (fun (_, institution) -> institution.FailureModes)
            |> Set.ofSeq

        Assert.Contains(BusBunching, modes)
        Assert.Contains(OvercrowdedClassrooms, modes)

    [<Fact>]
    let ``GeneratedWorldPassesValidation`` () =
        let world = TestWorld.create ()
        let failures =
            world.GenerationReport.Findings
            |> List.filter (fun finding -> finding.Status = ValidationStatus.Failed)

        Invariants.checkWorld world |> ignore
        Assert.Empty(failures)

    [<Fact>]
    let ``OccupiedUnitsAreReachable`` () =
        let world = TestWorld.create ()

        world.HousingUnits
        |> Map.toSeq
        |> Seq.filter (fun (_, unit) -> not unit.Vacancy)
        |> Seq.iter (fun (_, unit) ->
            unit.Occupants
            |> Seq.iter (fun householdId ->
                let household = world.Households[householdId]
                let home = world.Map.Places[household.Home]
                Assert.NotEqual(NoRoadAccess, home.RoadAccess)))

        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``RoadHierarchyIsConnected`` () =
        let world = TestWorld.create ()
        let nodes = world.Map.RoadNodes |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let start = nodes |> Seq.head

        let adjacency =
            world.Map.RoadSegments
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

        let reachable = visit Set.empty [ start ]
        Assert.Equal<int>(nodes.Count, reachable.Count)

    [<Fact>]
    let ``TransitRoutesHaveDemand`` () =
        let world = TestWorld.create ()
        let residentialPlaces =
            world.Map.Places
            |> Map.toSeq
            |> Seq.filter (fun (_, place) -> place.Kind = Residence)
            |> Seq.map fst
            |> Set.ofSeq

        let jobOrSchoolPlaces =
            world.Map.Places
            |> Map.toSeq
            |> Seq.filter (fun (_, place) -> place.Kind = Workplace || place.Kind = Industrial || place.Kind = School)
            |> Seq.map fst
            |> Set.ofSeq

        world.Transport.TransitRoutes
        |> Map.iter (fun _ route ->
            let servedPlaces =
                route.Stops
                |> List.choose (fun stopId -> world.Transport.TransitStops[stopId].Place)
                |> Set.ofList

            Assert.True(not (Set.intersect residentialPlaces servedPlaces).IsEmpty)
            Assert.True(not (Set.intersect jobOrSchoolPlaces servedPlaces).IsEmpty))

    [<Fact>]
    let ``InvalidWorldReportsValidationIssues`` () =
        let invalidWorld =
            { TestWorld.create () with
                Settlements = Map.empty }

        let findings = WorldGeneration.validate invalidWorld

        Assert.Contains(findings, fun finding ->
            finding.Status = ValidationStatus.Failed
            && finding.Issue = RoadHierarchyDisconnected
            && finding.Message.Contains("settlement"))
