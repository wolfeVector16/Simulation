namespace Simulation

open Simulation.Domain

module WorldIndexes =
    let private addUsage institutionId amount usages =
        Map.change institutionId (fun current -> Some(amount + Option.defaultValue 0 current)) usages

    let private schoolUsage world =
        world.Sims
        |> Map.toSeq
        |> Seq.choose (fun (personId, sim) ->
            sim.School
            |> Option.bind (fun enrollment ->
                world.Institutions
                |> Map.toSeq
                |> Seq.tryFind (fun (_, institution) -> institution.Place = Some enrollment.School)
                |> Option.map (fun (institutionId, _) -> institutionId, personId)))
        |> Seq.countBy fst
        |> Seq.map (fun (institutionId, count) -> institutionId, count)
        |> Map.ofSeq

    let private employerUsage world =
        world.GeneratedJobs
        |> Map.toSeq
        |> Seq.choose (fun (_, job) -> job.Employer)
        |> Seq.countBy id
        |> Map.ofSeq

    let private housingOwnerUsage world =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.choose (fun (_, unit) ->
            match unit.Owner with
            | InstitutionOwner institutionId ->
                let occupantCount =
                    unit.Occupants
                    |> Seq.sumBy (fun householdId ->
                        world.Households
                        |> Map.tryFind householdId
                        |> Option.map (fun household -> household.Members.Count)
                        |> Option.defaultValue 0)

                Some(institutionId, occupantCount)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (institutionId, rows) -> institutionId, rows |> Seq.sumBy snd)
        |> Map.ofSeq

    let private transitUsage world =
        let transitLoad =
            world.Transport.TransitRoutes
            |> Map.toSeq
            |> Seq.sumBy (fun (_, route) -> int (round (float route.Capacity * route.Crowding)))

        if transitLoad = 0 then
            Map.empty
        else
            world.Institutions
            |> Map.toSeq
            |> Seq.choose (fun (institutionId, institution) ->
                if institution.Kind = TransitInstitution then Some(institutionId, transitLoad) else None)
            |> Map.ofSeq

    let usedCapacityByInstitution world =
        [ schoolUsage world
          employerUsage world
          housingOwnerUsage world
          transitUsage world ]
        |> List.fold
            (fun usages usageMap ->
                usageMap |> Map.fold (fun usages institutionId amount -> addUsage institutionId amount usages) usages)
            Map.empty

    let usedCapacity institutionId world =
        usedCapacityByInstitution world
        |> Map.tryFind institutionId
        |> Option.defaultValue 0

    let institutionHasCapacity institutionId world =
        world.Institutions
        |> Map.tryFind institutionId
        |> Option.exists (fun institution -> usedCapacity institutionId world <= institution.Capacity)
