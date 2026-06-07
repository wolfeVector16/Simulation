namespace Simulation

open Simulation.Domain
open Simulation.Measures

module Diagnostics =
    let private severity score =
        if score >= 0.75 then Critical
        elif score >= 0.45 then Warning
        else Info

    let private risk area score message =
        { Area = area
          Severity = severity score
          Score = clamp01 score
          Message = message }

    let private averageOrZero values =
        let values = values |> Seq.toList
        if List.isEmpty values then 0.0 else List.average values

    let private householdFunds world =
        world.Households
        |> Map.toSeq
        |> Seq.map (fun (_, household) -> float household.Funds)
        |> Seq.toList

    let private inequalityScore world =
        let funds = householdFunds world |> List.sort

        match funds with
        | [] -> 0.0
        | [_] -> 0.20
        | values ->
            let low = values.Head
            let high = values |> List.last
            if high <= 0.0 then 0.20 else clamp01 ((high - low) / high)

    let private affordabilityScore world =
        let averageLandValue =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun parcel -> parcel.Zone = ResidentialZone)
            |> Seq.map _.LandValue
            |> averageOrZero

        let averageFunds = householdFunds world |> averageOrZero
        let fundsPressure = if averageFunds <= 0.0 then 1.0 else clamp01 (2500.0 / averageFunds)
        clamp01 (averageLandValue * 0.55 + fundsPressure * 0.45)

    let private maintenanceScore world =
        let householdDecay =
            world.Households
            |> Map.toSeq
            |> Seq.map (fun (_, household) ->
                let broken =
                    household.Objects
                    |> List.filter (fun obj -> obj.Broken)
                    |> List.length
                    |> float

                let objectPressure =
                    if household.Objects.IsEmpty then
                        0.0
                    else
                        broken / float household.Objects.Length

                clamp01 ((1.0 - household.Cleanliness) * 0.65 + objectPressure * 0.35))
            |> averageOrZero

        let cityDecay =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.map (fun parcel -> (parcel.FireRisk + parcel.Pollution + parcel.Crime) / 3.0)
            |> averageOrZero

        clamp01 (householdDecay * 0.45 + cityDecay * 0.55)

    let private psychologyScore world =
        let tenseOrSad =
            world.Sims
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun sim -> sim.Emotion = Tense || sim.Emotion = Sad || sim.Emotion = Angry)
            |> Seq.length
            |> float

        let population = max 1.0 (float world.Sims.Count)
        let lowNeeds =
            world.Sims
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.averageBy (fun sim ->
                sim.Needs
                |> Map.toSeq
                |> Seq.filter (fun (_, need) -> need.Value < need.CriticalBelow)
                |> Seq.length
                |> float)

        clamp01 (tenseOrSad / population * 0.45 + lowNeeds / 3.0 * 0.55 + 0.25)

    let private relationshipScore world =
        let relationshipCounts =
            world.Meta.Indexes.RelationshipIdsByPerson
            |> Map.toSeq
            |> Seq.map (fun (_, relationshipIds) -> float relationshipIds.Length)
            |> averageOrZero

        let sparsePenalty =
            if world.Relationships.IsEmpty then 0.45
            elif relationshipCounts < 2.0 then 0.25
            else 0.10

        let memoryPenalty = if world.Memories.IsEmpty then 0.20 else 0.10
        clamp01 (sparsePenalty + memoryPenalty)

    let private transportScore world =
        let activeTrips =
            world.Sims
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun sim -> match sim.Location with InTransit _ -> true | _ -> false)
            |> Seq.length
            |> float

        let tripPressure = activeTrips / max 1.0 (float world.Sims.Count)
        let trafficPressure = max world.City.Indicators.Traffic world.Transport.Metrics.AverageCongestion
        let unreliability = 1.0 - world.Transport.Metrics.AverageTravelReliability
        let parking = world.Transport.Metrics.AverageParkingPressure * 0.35
        let late = min 1.0 (float world.Transport.Metrics.LateArrivalsToday / 8.0)
        let emergency = world.Transport.Metrics.EmergencyResponseRisk

        clamp01 (0.18 + tripPressure * 0.18 + trafficPressure * 0.22 + unreliability * 0.20 + parking * 0.10 + late * 0.07 + emergency * 0.05)

    let private serviceQualityScore world =
        let indicators = world.City.Indicators
        clamp01 (0.35 + (1.0 - indicators.Education) * 0.22 + (1.0 - indicators.Health) * 0.18 + indicators.Crime * 0.15)

    let private capitalFinanceScore world =
        let demand = world.City.Demand
        let unbuiltDemand =
            [ demand.Residential; demand.Commercial; demand.Industrial ]
            |> List.average
            |> max 0.0

        let vacantUsefulParcels =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun parcel -> parcel.Building.IsNone && parcel.Zone <> Unzoned)
            |> Seq.length
            |> float

        clamp01 (0.30 + unbuiltDemand * 0.35 + min 1.0 (vacantUsefulParcels / 5.0) * 0.35)

    let tick world =
        let affordability = affordabilityScore world
        let inequality = inequalityScore world
        let maintenance = maintenanceScore world
        let psychology = psychologyScore world
        let relationship = relationshipScore world
        let transport = transportScore world
        let services = serviceQualityScore world
        let finance = capitalFinanceScore world

        let risks =
            [ risk PoliticsAndGovernance 0.72 "The city still acts like a benevolent-dictator planning model; councils, voters, lawsuits, procurement, and legitimacy are not yet actors."
              risk LandOwnership 0.66 "Housing units now have owners, rents, and legal status; parcels/lots still need ownership history, mortgages, speculation, and property-right constraints."
              risk HousingAffordability affordability "Rent pressure and household housing status exist, but displacement, subsidies, search friction, and homelessness need explicit mechanics."
              risk CapitalAndFinance finance "Development responds directly to demand; financing risk, interest rates, developer strategy, materials, and labor markets are not yet constraining projects."
              risk TransportBehavior transport "Transport now models modes, lanes, parking, access, and trip reliability; next gaps are richer incidents, microscopic vehicle promotion, and transit operations."
              risk ServiceQuality services "Services now have institution records; quality, trust, staffing, over/under-service, and institutional failure need stronger causal dynamics."
              risk Psychology psychology "Sims now accumulate event memories; habits, avoidance, burnout, trauma, and health shocks need to feed more decisions."
              risk RelationshipDepth relationship "Relationships now have multidimensional ties; interaction budgets, social history, group pressure, reputation, and conflict resolution need deeper dynamics."
              risk Inequality inequality "Household resources differ, but inheritance, credit access, discrimination, segregation, and social capital are not yet causal systems."
              risk MaintenanceAndDecay maintenance "Decay exists for households and city indicators, but long-lived infrastructure obligations, repair backlogs, and institutional rot need persistence."
              risk TimeCompression 0.62 "Construction, zoning, hiring, school quality, and behavior change still happen too quickly compared with institutional time."
              risk AgentConflict 0.70 "Actors do not yet negotiate or block each other: homeowners, renters, developers, employers, workers, parents, officials, and institutions need goals and leverage." ]
            |> List.sortByDescending _.Score

        { OverallFragility = risks |> List.averageBy _.Score
          Risks = risks }
