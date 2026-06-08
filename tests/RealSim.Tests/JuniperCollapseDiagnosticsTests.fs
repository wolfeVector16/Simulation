namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain
open RealSim.Scenarios

module JuniperCollapseDiagnosticsTests =
    let private runShortTrace () =
        Juniper.createWorld 1337
        |> CollapseDiagnostics.run 60 30

    let private parcelByName name world =
        world.City.Parcels
        |> Map.toSeq
        |> Seq.find (fun (_, parcel) -> parcel.Name = name)
        |> fst

    let private updateParcel parcelId updater (world: World) =
        { world with
            City =
                { world.City with
                    Parcels = world.City.Parcels |> Map.change parcelId (Option.map updater) } }

    let private forceNoUtilities (world: World) =
        { world with
            City =
                { world.City with
                    Utilities = world.City.Utilities |> List.map (fun utility -> { utility with Capacity = 0.0 }) } }

    let private setBuildingStatus (status: BuildingStatus) (parcel: Parcel) =
        { parcel with Building = parcel.Building |> Option.map (fun building -> { building with Status = status }) }

    [<Fact>]
    let ``JuniperCollapseTraceCanRun`` () =
        let trace = runShortTrace ()

        Assert.NotEmpty(trace.Points)
        Assert.Contains("Juniper collapse trace", trace.Report)

    [<Fact>]
    let ``JuniperJobsDoNotDivergeFromDerivedJobsBeforeCollapse`` () =
        let trace = runShortTrace ()

        let divergence =
            trace.Points
            |> List.tryFind (fun point -> point.CityJobsMetric <> point.DerivedJobCount)

        Assert.True(divergence.IsNone, "City.Indicators.Jobs should derive from canonical GeneratedJobs throughout the trace.")

    [<Fact>]
    let ``JuniperPopulationDoesNotDivergeFromDerivedPopulationBeforeCollapse`` () =
        let trace = runShortTrace ()

        let divergence =
            trace.Points
            |> List.tryFind (fun point -> point.CityPopulationMetric <> point.SimCount)

        Assert.True(divergence.IsNone, "City.Indicators.Population should derive from canonical Sims throughout the trace.")

    [<Fact>]
    let ``JuniperCollapseFirstCauseIsReported`` () =
        let trace = runShortTrace ()

        Assert.Contains("First dashboard jobs decrease", trace.Report)
        Assert.Contains("First dashboard population decrease", trace.Report)
        Assert.Contains("Root cause classification", trace.Report)

    [<Fact>]
    let ``NoSystemAccidentallyClearsJobs`` () =
        let before = Juniper.createWorld 1337
        let after = Simulation.Engine.tick 60 before

        Assert.True(after.GeneratedJobs.Count > 0, "Generated jobs were unexpectedly cleared after one tick.")
        Assert.True(after.GeneratedJobs.Count >= before.GeneratedJobs.Count)
        Assert.Contains(after.Meta.EventLog, function
            | JobsCreated _ -> true
            | _ -> false)
        Assert.Equal(before.Institutions.Count, after.Institutions.Count)

    [<Fact>]
    let ``NoSystemAccidentallyClearsHouseholdsOrSims`` () =
        let before = Juniper.createWorld 1337
        let after = Simulation.Engine.tick 60 before

        Assert.True(after.Sims.Count >= before.Sims.Count)
        Assert.True(after.Households.Count >= before.Households.Count)
        Assert.True(after.HousingUnits.Count >= before.HousingUnits.Count)

        if after.HousingUnits.Count > before.HousingUnits.Count then
            Assert.Contains(after.Meta.EventLog, function
                | HousingUnitsAdded _ -> true
                | _ -> false)

    [<Fact>]
    let ``JobLossEventsMatchJobCountDrop`` () =
        let trace = runShortTrace ()

        Assert.DoesNotContain(trace.CauseSummaries, fun summary -> List.contains "Unexplained dashboard job loss" summary.Warnings)

    [<Fact>]
    let ``PopulationLossEventsMatchPopulationDrop`` () =
        let trace = runShortTrace ()

        Assert.DoesNotContain(trace.CauseSummaries, fun summary -> List.contains "Unexplained dashboard population loss" summary.Warnings)

    [<Fact>]
    let ``CityPopulationMetricDerivesFromCanonicalSimsOrHouseholds`` () =
        let world = Juniper.createWorld 1337

        Assert.Equal(CityMetrics.derivePopulation world, world.City.Indicators.Population)

    [<Fact>]
    let ``CityJobsMetricDerivesFromCanonicalJobsOrEmployers`` () =
        let world = Juniper.createWorld 1337

        Assert.Equal(CityMetrics.deriveJobs world, world.City.Indicators.Jobs)

    [<Fact>]
    let ``AbandonedParcelDoesNotErasePopulationMetricWithoutDisplacement`` () =
        let world = Juniper.createWorld 1337
        let beforePopulation = CityMetrics.derivePopulation world
        let parcelId = parcelByName "Old Rowhouse Block" world

        let after =
            world
            |> updateParcel parcelId (setBuildingStatus BuildingStatus.Abandoned)
            |> CityMetrics.updateIndicators

        Assert.Equal(beforePopulation, after.City.Indicators.Population)
        Assert.Equal(beforePopulation, CityMetrics.derivePopulation after)

    [<Fact>]
    let ``AbandonedCommercialParcelDoesNotEraseJobsMetricWithoutBusinessClosure`` () =
        let world = Juniper.createWorld 1337
        let beforeJobs = CityMetrics.deriveJobs world
        let parcelId = parcelByName "Civic Office Row" world

        let after =
            world
            |> updateParcel parcelId (setBuildingStatus BuildingStatus.Abandoned)
            |> CityMetrics.updateIndicators

        Assert.Equal(beforeJobs, after.City.Indicators.Jobs)
        Assert.Equal(beforeJobs, CityMetrics.deriveJobs after)

    [<Fact>]
    let ``BuildingAbandonmentEmitsEvent`` () =
        let parcelId = Juniper.createWorld 1337 |> parcelByName "Civic Office Row"

        let world =
            Juniper.createWorld 1337
            |> forceNoUtilities
            |> updateParcel parcelId (setBuildingStatus BuildingStatus.Vacant)

        let after = Simulation.Engine.tick 60 world

        Assert.Contains(after.Meta.EventLog, function
            | BuildingAbandoned(_, _, BuildingId abandonedParcel) -> abandonedParcel = parcelId
            | _ -> false)

    [<Fact>]
    let ``OccupiedResidentialBuildingDoesNotAbandonSilently`` () =
        let world = Juniper.createWorld 1337
        let parcelId = parcelByName "Old Rowhouse Block" world

        let after =
            world
            |> forceNoUtilities
            |> Simulation.Engine.tick 60

        let status = after.City.Parcels[parcelId].Building |> Option.map _.Status
        Assert.NotEqual(Some BuildingStatus.Abandoned, status)
        Assert.DoesNotContain(after.Meta.EventLog, function
            | BuildingAbandoned(_, _, BuildingId abandonedParcel) -> abandonedParcel = parcelId
            | _ -> false)

    [<Fact>]
    let ``ActiveEmployerBuildingDoesNotAbandonSilently`` () =
        let world = Juniper.createWorld 1337
        let parcelId = parcelByName "Civic Office Row" world

        let after =
            world
            |> forceNoUtilities
            |> Simulation.Engine.tick 60

        let status = after.City.Parcels[parcelId].Building |> Option.map _.Status
        Assert.NotEqual(Some BuildingStatus.Abandoned, status)
        Assert.Equal(CityMetrics.deriveJobs world, after.City.Indicators.Jobs)
        Assert.DoesNotContain(after.Meta.EventLog, function
            | JobsLost _
            | BusinessClosed _ -> true
            | BuildingAbandoned(_, _, BuildingId abandonedParcel) when abandonedParcel = parcelId -> true
            | _ -> false)

    [<Fact>]
    let ``AbandonmentRequiresPersistentLowDesirability`` () =
        let world = Juniper.createWorld 1337 |> forceNoUtilities
        let parcelId = parcelByName "Main Street Retail" world

        let after = Simulation.Engine.tick 60 world

        let status = after.City.Parcels[parcelId].Building |> Option.map _.Status
        Assert.NotEqual(Some BuildingStatus.Abandoned, status)

    [<Fact>]
    let ``CollapseTraceNoUnexplainedDashboardJobLoss`` () =
        let trace = runShortTrace ()

        Assert.DoesNotContain(trace.CauseSummaries, fun summary -> List.contains "Unexplained dashboard job loss" summary.Warnings)

    [<Fact>]
    let ``CollapseTraceNoUnexplainedDashboardPopulationLoss`` () =
        let trace = runShortTrace ()

        Assert.DoesNotContain(trace.CauseSummaries, fun summary -> List.contains "Unexplained dashboard population loss" summary.Warnings)

    [<Fact>]
    let ``JuniperDashboardDoesNotHitZeroWhileCanonicalStateNonZero`` () =
        let trace = runShortTrace ()

        Assert.DoesNotContain(trace.Points, fun point -> point.DerivedJobCount > 0 && point.CityJobsMetric = 0)
        Assert.DoesNotContain(trace.Points, fun point -> point.SimCount > 0 && point.CityPopulationMetric = 0)

    [<Fact>]
    let ``OrganicDevelopmentCreatesCanonicalJobsFromSupportedParcels`` () =
        let before = Juniper.createWorld 1337
        let after = Simulation.Engine.tick 60 before

        Assert.True(after.GeneratedJobs.Count > before.GeneratedJobs.Count, "Supported commercial/industrial parcels should materialize canonical jobs.")
        Assert.Contains(after.Meta.EventLog, function
            | JobsCreated _ -> true
            | _ -> false)
        Assert.Equal(CityMetrics.deriveJobs after, after.City.Indicators.Jobs)

    [<Fact>]
    let ``OrganicDevelopmentCreatesVacantHousingUnitsFromSupportedResidentialDemand`` () =
        let before = Juniper.createWorld 1337
        let after = Simulation.Engine.tick 60 before

        Assert.True(after.HousingUnits.Count > before.HousingUnits.Count, "Supported residential buildings should materialize canonical housing units.")
        Assert.Contains(after.Meta.EventLog, function
            | HousingUnitsAdded _ -> true
            | _ -> false)

        let newVacantUnits =
            after.HousingUnits.Count - before.HousingUnits.Count

        Assert.True(newVacantUnits > 0)

    [<Fact>]
    let ``OrganicMigrationCreatesHouseholdWhenHousingJobsAndDemandSupportIt`` () =
        let before = Juniper.createWorld 1337
        let after = TestWorld.runTicks 60 6 before

        Assert.True(after.Households.Count > before.Households.Count, "Migration should add a canonical household during the supported morning window.")
        Assert.True(after.Sims.Count > before.Sims.Count, "Migration should add a canonical resident.")
        Assert.Contains(after.Meta.EventLog, function
            | HouseholdCreated _ -> true
            | _ -> false)
        Assert.Contains(after.Meta.EventLog, function
            | HouseholdMovedIn _ -> true
            | _ -> false)
        Assert.Contains(after.Meta.EventLog, function
            | JobStarted _ -> true
            | _ -> false)
        Assert.Equal(CityMetrics.derivePopulation after, after.City.Indicators.Population)

    [<Fact>]
    let ``JuniperCanGrowWhenZoningAccessAndDemandSupportIt`` () =
        let before = Juniper.createWorld 1337
        let after = TestWorld.runTicks 60 (30 * 24) before

        Assert.True(after.GeneratedJobs.Count > before.GeneratedJobs.Count)
        Assert.True(after.HousingUnits.Count > before.HousingUnits.Count)
        Assert.True(after.Households.Count > before.Households.Count)
        Assert.True(after.Sims.Count > before.Sims.Count)
        Assert.Equal(CityMetrics.deriveJobs after, after.City.Indicators.Jobs)
        Assert.Equal(CityMetrics.derivePopulation after, after.City.Indicators.Population)
