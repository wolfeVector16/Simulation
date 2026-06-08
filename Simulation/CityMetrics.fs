namespace Simulation

open Simulation.Domain
open Simulation.Measures

module CityMetrics =
    /// Canonical population authority: the resident Sim table.
    /// Parcel/building occupants are capacity/planning data and must not drive dashboard population.
    let derivePopulation (world: World) = world.Sims.Count

    /// Canonical job authority: the GeneratedJobs table until a richer employer/business ledger exists.
    /// Parcel building job counts are floor-space/capacity estimates and must not drive dashboard jobs.
    let deriveJobs (world: World) = world.GeneratedJobs.Count

    let deriveFilledJobs (world: World) =
        world.Sims
        |> Map.toSeq
        |> Seq.filter (fun (_, sim) -> sim.Job.IsSome)
        |> Seq.length

    let deriveOpenJobs world =
        max 0 (deriveJobs world - deriveFilledJobs world)

    let deriveHouseholdCount (world: World) = world.Households.Count

    let deriveBusinessCount (world: World) =
        world.Map.Places
        |> Map.toSeq
        |> Seq.filter (fun (_, place) -> place.Kind = Commercial || place.Kind = Industrial || place.Kind = Workplace)
        |> Seq.length

    let deriveUnemployment world =
        let laborForce =
            world.Sims
            |> Map.toSeq
            |> Seq.filter (fun (_, sim) -> sim.LifeStage = YoungAdult || sim.LifeStage = Adult || sim.LifeStage = Elder)
            |> Seq.length

        if laborForce = 0 then
            0.0
        else
            max 0.0 (float (laborForce - deriveFilledJobs world) / float laborForce)

    let updateIndicators world =
        let indicators =
            { world.City.Indicators with
                Population = derivePopulation world
                Jobs = deriveJobs world
                Unemployment = deriveUnemployment world }

        { world with City = { world.City with Indicators = indicators } }
