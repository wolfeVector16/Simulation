namespace Simulation

open Simulation.Domain

module WorldGeneration =
    let generationSteps =
        [ GenerateGeography
          GenerateSettlements
          GenerateTransportCorridors
          GenerateDistricts
          GenerateRoadHierarchy
          GenerateLandUse
          GenerateBlocks
          GenerateParcels
          GenerateBuildings
          GenerateInstitutions
          GenerateHouseholds
          GenerateSocialGraph
          GenerateTransit
          GenerateEconomy
          ValidateWorld ]

    let placeholderReport seed scenario =
        { Seed = seed
          Scenario = scenario
          Steps = generationSteps
          Assumptions = []
          GeneratedSummary = []
          Findings = []
          Repairs = []
          IntentionalConstraints = [] }

    let private finding issue status message repair =
        { Issue = issue
          Status = status
          Message = message
          RepairAction = repair }

    let private placeHasAccess world placeId =
        world.Map.Places
        |> Map.tryFind placeId
        |> Option.exists (fun place -> place.RoadAccess <> NoRoadAccess)

    let private occupiedHousingFindings world =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.collect (fun (_, unit) ->
            unit.Occupants
            |> Seq.choose (fun householdId ->
                match Map.tryFind householdId world.Households with
                | Some household when not (placeHasAccess world household.Home) ->
                    Some(finding (ImplausibleCommutePattern householdId) ValidationStatus.Failed $"%s{household.Name} occupies housing without road or pedestrian access." (Some "Assign the household to a unit with access or generate a pedestrian connector."))
                | _ -> None))
        |> Seq.toList

    let private schoolFindings world =
        world.Sims
        |> Map.toSeq
        |> Seq.choose (fun (_, sim) ->
            match sim.LifeStage, sim.School with
            | Child, None
            | Teen, None ->
                Some(finding (UnreachableSchool sim.Household) ValidationStatus.Failed $"%s{sim.Name} is school-aged but has no school assignment." (Some "Assign a catchment school or mark explicit lack of access."))
            | (Child | Teen), Some school when not (placeHasAccess world school.School) ->
                Some(finding (UnreachableSchool sim.Household) ValidationStatus.Failed $"%s{sim.Name}'s school is not reachable from the generated network." (Some "Move school into catchment or add a collector/transit connection."))
            | _ -> None)
        |> Seq.toList

    let private jobFindings world =
        world.Sims
        |> Map.toSeq
        |> Seq.choose (fun (_, sim) ->
            match sim.Job with
            | Some job ->
                match MapGraph.findRoute world.Map sim.Home job.Workplace with
                | Some route when route.TotalMinutes <= 90.0 -> None
                | Some route ->
                    let message = sprintf "%s's commute is long at %.0f minutes." sim.Name route.TotalMinutes
                    Some(finding (ImplausibleCommutePattern sim.Household) ValidationStatus.IntentionalConstraint message (Some "Scenario allows long cross-town commutes; transport stress will model the consequence."))
                | None ->
                    Some(finding (ImplausibleCommutePattern sim.Household) ValidationStatus.Failed $"%s{sim.Name} has an unreachable job." (Some "Regenerate job placement or add regional connector."))
            | None -> None)
        |> Seq.toList

    let private laneFindings world =
        world.Transport.Lanes
        |> Map.toSeq
        |> Seq.choose (fun (laneId, lane) ->
            if lane.PermittedMovements.IsEmpty || lane.AllowedModes.IsEmpty then
                Some(finding (InvalidLaneMovement laneId) ValidationStatus.Failed "A generated lane has no valid movement or mode." (Some "Regenerate lane from road class defaults."))
            else
                None)
        |> Seq.toList

    let private transitFindings world =
        world.Transport.TransitRoutes
        |> Map.toSeq
        |> Seq.choose (fun (routeId, route) ->
            if route.Stops.Length < 2 then
                Some(finding (TransitRouteWithoutDemand routeId) ValidationStatus.Failed $"%s{route.Name} has fewer than two stops." (Some "Remove route or connect plausible origins and destinations."))
            elif route.HeadwayMinutes > 45 then
                Some(finding (TransitRouteWithoutDemand routeId) ValidationStatus.IntentionalConstraint $"%s{route.Name} has poor headways; this is retained as a transit reliability constraint." None)
            else
                None)
        |> Seq.toList

    let private emergencyFindings world =
        world.Transport.AccessByNeighborhood
        |> Map.toSeq
        |> Seq.choose (fun (neighborhoodId, access) ->
            if access.EmergencyAccess < 0.45 then
                let name = world.Neighborhoods |> Map.tryFind neighborhoodId |> Option.map _.Name |> Option.defaultValue "Unknown neighborhood"
                Some(finding (EmergencyCoverageGap neighborhoodId) ValidationStatus.IntentionalConstraint $"%s{name} has weak emergency access." (Some "Scenario keeps this as a service planning problem."))
            else
                None)
        |> Seq.toList

    let private institutionFindings world =
        world.Institutions
        |> Map.toSeq
        |> Seq.choose (fun (institutionId, institution) ->
            if not (Map.containsKey institution.Neighborhood world.Neighborhoods) then
                Some(finding (InstitutionWithoutCatchment institutionId) ValidationStatus.Failed $"%s{institution.Name} is not assigned to a valid neighborhood catchment." (Some "Assign institution to nearest district/neighborhood."))
            else
                None)
        |> Seq.toList

    let private blockFindings world =
        world.Blocks
        |> Map.toSeq
        |> Seq.collect (fun (_, block) ->
            block.Parcels
            |> Seq.choose (fun parcelId ->
                if not (Map.containsKey parcelId world.City.Parcels) then
                    Some(finding (IsolatedParcel parcelId) ValidationStatus.Failed $"%s{block.Name} references a parcel that was not generated." (Some "Regenerate block parcel membership from city parcel table."))
                elif block.RoadFrontage.IsEmpty && block.Buildable then
                    Some(finding (IsolatedParcel parcelId) ValidationStatus.Failed $"%s{block.Name} has a buildable parcel without road frontage." (Some "Attach local street frontage or mark block open space."))
                else
                    None))
        |> Seq.toList

    let validate world =
        [ if world.Map.RoadSegments.IsEmpty then
              finding RoadHierarchyDisconnected ValidationStatus.Failed "No generated road hierarchy exists." (Some "Generate regional corridors before land use.")
          if world.Settlements.IsEmpty then
              finding RoadHierarchyDisconnected ValidationStatus.Failed "No settlement structure was generated before simulation." (Some "Generate settlement archetypes before roads/parcels.") ]
        @ occupiedHousingFindings world
        @ schoolFindings world
        @ jobFindings world
        @ laneFindings world
        @ transitFindings world
        @ emergencyFindings world
        @ institutionFindings world
        @ blockFindings world

    let refreshReport world =
        let findings = validate world
        let repairs =
            findings
            |> List.choose (fun finding -> finding.RepairAction)
            |> List.distinct

        let constraints =
            findings
            |> List.filter (fun finding -> finding.Status = ValidationStatus.IntentionalConstraint)
            |> List.map _.Message

        { world.GenerationReport with
            Assumptions =
                [ "World generated in layers: geography, settlements, corridors, districts, land use, parcels/buildings, institutions, households, social graph, transit, economy, validation."
                  "Scenario uses an old industrial river-city pattern: downtown grid, aging apartments, industrial edge, regional mall, and constrained transit."
                  "Existing procedural and future imported worlds share the same World/CityMap/TransportState domain types." ]
            GeneratedSummary =
                [ $"%s{world.Region.Name}: %d{world.Settlements.Count} settlements, %d{world.Districts.Count} districts, %d{world.Neighborhoods.Count} neighborhoods."
                  $"%d{world.Blocks.Count} blocks, %d{world.City.Parcels.Count} parcels, %d{world.HousingUnits.Count} housing units."
                  $"%d{world.Map.RoadSegments.Length} road segments, %d{world.Transport.Lanes.Count} lanes, %d{world.Transport.Intersections.Count} intersections."
                  $"%d{world.Institutions.Count} institutions, %d{world.Households.Count} households, %d{world.Sims.Count} people." ]
            Findings = findings
            Repairs = repairs
            IntentionalConstraints = constraints }
