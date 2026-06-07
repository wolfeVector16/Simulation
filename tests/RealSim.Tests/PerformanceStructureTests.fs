namespace RealSim.Tests

open System.IO
open Xunit
open Simulation
open Simulation.Domain

module PerformanceStructureTests =
    let private sourceRoot =
        let cwd = Directory.GetCurrentDirectory()
        let rec findRoot dir =
            let candidate = Path.Combine(dir, "Simulation.slnx")
            if File.Exists(candidate) then
                dir
            else
                let parent = Directory.GetParent(dir)
                if isNull parent then cwd else findRoot parent.FullName

        findRoot cwd

    let private simulationSources () =
        Directory.GetFiles(Path.Combine(sourceRoot, "Simulation"), "*.fs", SearchOption.TopDirectoryOnly)
        |> Array.filter (fun path -> Path.GetFileName(path) <> "Program.fs")

    [<Fact>]
    let ``SimulationModulesDoNotUseGlobalRandomnessOrWallClock`` () =
        let banned =
            [ "Guid.NewGuid("
              "DateTime.Now"
              "DateTime.UtcNow"
              "Random("
              "System.Random" ]

        let offenders =
            simulationSources ()
            |> Array.collect (fun path ->
                let text = File.ReadAllText(path)
                banned
                |> List.choose (fun token ->
                    if text.Contains(token) then Some($"{Path.GetFileName(path)} contains {token}") else None)
                |> List.toArray)

        Assert.Empty(offenders)

    [<Fact>]
    let ``RuntimeIndexesAreDenseAndStable`` () =
        let world = TestWorld.create ()

        Assert.Equal(world.Sims.Count, world.Runtime.PersonIdsByIndex.Length)
        Assert.Equal(world.Households.Count, world.Runtime.HouseholdIdsByIndex.Length)
        Assert.Equal(world.Transport.Lanes.Count, world.Runtime.LaneIdsByIndex.Length)
        Assert.Equal(world.Sims.Count, world.Runtime.NeedsByPersonIndex.Length)
        Assert.Equal<SimId list>(world.Runtime.PersonIdsByIndex |> Array.toList, world.Runtime.PersonIdsByIndex |> Array.sort |> Array.toList)
        Assert.Equal<LaneId list>(world.Runtime.LaneIdsByIndex |> Array.toList, world.Runtime.LaneIdsByIndex |> Array.sort |> Array.toList)

        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``PerformanceBudgetsAreExplicitAndBounded`` () =
        let world = TestWorld.create ()

        Assert.InRange(world.Performance.MaxCandidateActionsPerPersonPerTick, 1, 8)
        Assert.InRange(world.Performance.MaxRouteAlternativesPerTrip, 1, 4)
        Assert.InRange(world.Performance.MaxMemoriesInspectedPerDecision, 1, 32)
        Assert.InRange(world.Performance.MaxActiveTripsPerPartitionBeforeAggregation, 1, 512)

    [<Fact>]
    let ``RouteCacheKeysProduceStableRoutes`` () =
        let world = TestWorld.create ()
        let sim = world.Sims |> Map.toSeq |> Seq.map snd |> Seq.find (fun sim -> sim.Job.IsSome)
        let workplace = sim.Job.Value.Workplace
        let route1 = MapGraph.findRoute world.Map sim.Home workplace
        let route2 = MapGraph.findRoute world.Map sim.Home workplace

        Assert.Equal(sprintf "%A" route1, sprintf "%A" route2)

    [<Fact>]
    let ``RuntimeCacheInvalidationVersionCanChangeDeterministically`` () =
        let world = TestWorld.create ()
        let changed =
            { world with
                Runtime =
                    { world.Runtime with
                        RouteCache = Map.empty
                        TravelTimeCache = Map.empty
                        CacheVersion = world.Runtime.CacheVersion + 1 } }

        Assert.Equal(world.Runtime.CacheVersion + 1, changed.Runtime.CacheVersion)
        Assert.Empty(changed.Runtime.RouteCache)
        Assert.Empty(changed.Runtime.TravelTimeCache)

    [<Fact>]
    let ``RoutineTickKeepsPartitionWorkloadsBounded`` () =
        let world = TestWorld.create () |> TestWorld.runTicks 15 16

        world.PerformanceDiagnostics.PartitionWorkloads
        |> Map.iter (fun _ workload ->
            Assert.True(workload <= world.Performance.MaxActiveTripsPerPartitionBeforeAggregation))

        Assert.True(world.PerformanceDiagnostics.RouteCalculations >= 0)
        Invariants.checkWorld world |> ignore
