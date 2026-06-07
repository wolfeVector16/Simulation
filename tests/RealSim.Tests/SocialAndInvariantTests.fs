namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain

module SocialAndInvariantTests =
    [<Fact>]
    let ``SocialGraphIsSparse`` () =
        let world = TestWorld.create ()
        let simCount = world.Sims.Count
        let possibleDirectedEdges = simCount * max 0 (simCount - 1)

        Assert.True(world.Relationships.Count < possibleDirectedEdges, "Seeded social graph should be sparse, not all-to-all.")
        Invariants.checkWorld world |> ignore

    [<Fact>]
    let ``HouseholdCreatesStrongTies`` () =
        let world = TestWorld.create ()

        let familyEdges =
            world.Relationships
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun edge -> Set.contains ParentOf edge.Kinds || Set.contains ChildOf edge.Kinds)
            |> Seq.toList

        Assert.NotEmpty(familyEdges)
        Assert.All(familyEdges, fun edge -> Assert.Equal(CloseTie, edge.Strength))

    [<Fact>]
    let ``WorkplaceCreatesCoworkerOrWorkGroupContext`` () =
        let world = TestWorld.create ()
        let workGroups =
            world.Groups
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun group -> group.Kind = WorkTeam)
            |> Seq.toList

        Assert.NotEmpty(workGroups)
        Assert.All(workGroups, fun group -> Assert.NotEmpty(group.Members))

    [<Fact>]
    let ``CandidateInteractionsAreBounded`` () =
        let world = TestWorld.create ()

        world.Sims
        |> Map.iter (fun simId sim ->
            let relationshipCount =
                world.Meta.Indexes.RelationshipIdsByPerson
                |> Map.tryFind simId
                |> Option.defaultValue []
                |> List.length

            Assert.True(relationshipCount <= sim.SocialCapacity, $"{sim.Name} exceeds social attention budget."))

    [<Fact>]
    let ``InstitutionsDoNotExceedCapacity`` () =
        let world = TestWorld.create ()

        world.Institutions
        |> Map.iter (fun institutionId institution ->
            Assert.True(WorldIndexes.usedCapacity institutionId world <= institution.Capacity, $"{institution.Name} exceeds capacity."))

    [<Fact>]
    let ``MutatedWorldPreservesCentralInvariants`` () =
        let world = TestWorld.create () |> TestWorld.runTicks 15 24

        Invariants.checkWorld world |> ignore
        Assert.True(world.Meta.EventLog.Length > 0)
