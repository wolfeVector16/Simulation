namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain

module DeterminismTests =
    [<Fact>]
    let ``SameSeedProducesSameWorld`` () =
        let world1 = TestWorld.create ()
        let world2 = TestWorld.create ()

        Invariants.checkWorld world1 |> ignore
        Invariants.checkWorld world2 |> ignore

        Assert.Equal(sprintf "%A" world1.Region, sprintf "%A" world2.Region)
        Assert.Equal(sprintf "%A" world1.Settlements, sprintf "%A" world2.Settlements)
        Assert.Equal(sprintf "%A" world1.Map.RoadSegments, sprintf "%A" world2.Map.RoadSegments)
        Assert.Equal(sprintf "%A" world1.Transport.Lanes, sprintf "%A" world2.Transport.Lanes)
        Assert.Equal(sprintf "%A" world1.GenerationReport, sprintf "%A" world2.GenerationReport)

    [<Fact>]
    let ``SameSeedProducesSameEventLog`` () =
        let result1 = TestWorld.create () |> TestWorld.runTicks 15 32
        let result2 = TestWorld.create () |> TestWorld.runTicks 15 32

        Invariants.checkWorld result1 |> ignore
        Invariants.checkWorld result2 |> ignore

        Assert.Equal<string list>(TestWorld.eventLogText result1, TestWorld.eventLogText result2)

    [<Fact>]
    let ``ConflictResolutionIsDeterministic`` () =
        let prepare world =
            { world with
                MinuteOfDay = 7 * 60
                Households =
                    world.Households
                    |> Map.map (fun _ household ->
                        { household with
                            Funds = 10m
                            BillsDue = 100m
                            Stability = 0.40 }) }

        let result1 = TestWorld.create () |> prepare |> SimulationPipeline.tick
        let result2 = TestWorld.create () |> prepare |> SimulationPipeline.tick

        Invariants.checkWorld result1 |> ignore
        Assert.Equal<string list>(TestWorld.eventLogText result1, TestWorld.eventLogText result2)
        Assert.Equal<string list>(TestWorld.decisionsText result1, TestWorld.decisionsText result2)

    [<Fact>]
    let ``ParallelAndSingleThreadedTicksMatch`` () =
        let initial = TestWorld.create ()

        let branchA = initial |> TestWorld.runTicks 15 16
        let branchB = initial |> TestWorld.runTicks 15 16

        Invariants.checkWorld branchA |> ignore
        Assert.Equal<string list>(TestWorld.eventLogText branchA, TestWorld.eventLogText branchB)
        Assert.Equal(branchA.City.Budget.Treasury, branchB.City.Budget.Treasury)

    [<Fact>]
    let ``EventReplayRecreatesWorldState`` () =
        let initial = TestWorld.create ()
        let simId, _ = initial.Sims |> Map.toSeq |> Seq.head
        let householdId, household = initial.Households |> Map.toSeq |> Seq.head

        let events =
            [ RentIncreased(TestIds.eventId 21, householdId, household.RentMonthly |> Option.defaultValue 1000m, 1775m)
              TransportEventOccurred(TestIds.eventId 22, ArrivedLate(simId, WorkTrip, 18)) ]

        let replay1 = SimulationPipeline.applyEvents events initial
        let replay2 = SimulationPipeline.applyEvents events initial

        Invariants.checkWorld replay1 |> ignore
        Assert.Equal(sprintf "%A" replay1.Households, sprintf "%A" replay2.Households)
        Assert.Equal(sprintf "%A" replay1.Sims, sprintf "%A" replay2.Sims)
        Assert.Equal(sprintf "%A" replay1.Memories, sprintf "%A" replay2.Memories)
