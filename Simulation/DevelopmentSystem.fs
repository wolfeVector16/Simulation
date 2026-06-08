namespace Simulation

open System
open System.Security.Cryptography
open System.Text
open Simulation.Domain
open Simulation.Measures

module DevelopmentSystem =
    let private stableGuid parts =
        let text = String.concat "|" parts
        let bytes = Encoding.UTF8.GetBytes text
        let hash = SHA256.HashData bytes
        let guidBytes = Array.zeroCreate<byte> 16
        Array.Copy(hash, guidBytes, 16)
        Guid(guidBytes)

    let private guid world label index =
        stableGuid [ string world.Meta.Seed; string world.Meta.Tick; label; string index ]

    let private eventId world label index =
        EventId(guid world label index)

    let private neighborhoodForParcel world parcelId =
        world.Blocks
        |> Map.toSeq
        |> Seq.tryPick (fun (_, block) ->
            if Set.contains parcelId block.Parcels then
                world.Districts
                |> Map.tryFind block.District
                |> Option.bind (fun district -> district.Neighborhoods |> Seq.tryHead)
            else
                None)
        |> Option.orElseWith (fun () -> world.Neighborhoods |> Map.toSeq |> Seq.tryHead |> Option.map fst)

    let private placeForParcel (world: World) parcelId (parcel: Parcel) (building: Building) =
        let existing =
            world.Map.Places
            |> Map.toSeq
            |> Seq.tryFind (fun (_, (place: Place)) ->
                match building.Use, place.Kind with
                | Housing, Residence -> MapGraph.distanceMeters world.Map place.Position parcel.Position < 220.0
                | Commerce, Commercial -> MapGraph.distanceMeters world.Map place.Position parcel.Position < 260.0
                | Industry, Industrial -> MapGraph.distanceMeters world.Map place.Position parcel.Position < 320.0
                | _ -> false)
            |> Option.map fst

        match existing with
        | Some placeId -> world, placeId
        | None ->
            let placeId = PlaceId(guid world "development-place" (parcelId.GetHashCode()))
            let kind =
                match building.Use with
                | Housing -> Residence
                | Commerce -> Commercial
                | Industry -> Industrial
                | PublicService -> Civic
                | Recreation -> Park

            let place: Place =
                { Id = placeId
                  Name = building.Name
                  Kind = kind
                  Position = parcel.Position
                  RoadAccess = NearestRoadAccess 800.0
                  Economy = None }

            { world with Map = { world.Map with Places = Map.add placeId place world.Map.Places } }, placeId

    let private occupiedUnitCapacity world neighborhoodId =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.filter (fun (_, unit) -> unit.Neighborhood = neighborhoodId)
        |> Seq.sumBy (fun (_, unit) -> unit.SoftCapacity)

    let private materializeHousing (world: World) =
        let maxUnitsPerTick = 3

        let folder (world, events, added) (parcelId, (parcel: Parcel)) =
            match parcel.Building, neighborhoodForParcel world parcelId with
            | Some building, Some neighborhoodId
                when building.Use = Housing
                     && building.Status = Occupied
                     && parcel.RoadConnected
                     && parcel.Powered
                     && parcel.Watered
                     && world.City.Demand.Residential > 0.15
                     && added < maxUnitsPerTick ->
                let existingCapacity = occupiedUnitCapacity world neighborhoodId
                let targetCapacity = building.Capacity

                if existingCapacity < targetCapacity then
                    let worldWithPlace, _ = placeForParcel world parcelId parcel building

                    let unitId = UnitId(guid worldWithPlace "development-unit" (parcelId.GetHashCode() + added))
                    let lotId = LotId(guid worldWithPlace "development-lot" (parcelId.GetHashCode() + added))
                    let softCapacity = min 4 (max 1 (targetCapacity - existingCapacity))
                    let rent =
                        let baseRent = 850m + decimal (parcel.LandValue * 900.0)
                        if building.Wealth = HighWealth then baseRent + 500m
                        elif building.Wealth = LowWealth then max 650m (baseRent - 250m)
                        else baseRent

                    let unit =
                        { Id = unitId
                          Lot = lotId
                          Neighborhood = neighborhoodId
                          Owner = CorporateOwner
                          Occupants = Set.empty
                          RentMonthly = Some rent
                          MortgageMonthly = None
                          Condition = clamp01 (parcel.Desirability + 0.25)
                          SoftCapacity = softCapacity
                          HardCapacity = softCapacity + 2
                          UtilityAccess = [ PowerUtility; WaterUtility; SewageUtility; GarbageUtility ] |> Set.ofList
                          LegalStatus = LeaseActive
                          Habitability = clamp01 (parcel.Desirability + 0.35)
                          EvictionRisk = 0.05
                          Vacancy = true }

                    let world =
                        { worldWithPlace with
                            HousingUnits = Map.add unitId unit world.HousingUnits
                            Neighborhoods =
                                world.Neighborhoods
                                |> Map.change neighborhoodId (Option.map (fun n ->
                                    { n with
                                        Lots = Set.add lotId n.Lots
                                        VacancyRate = clamp01 (n.VacancyRate + 0.02) })) }

                    world, HousingUnitsAdded(eventId world "housing-units-added" added, SimulationSystemCommand EconomySystem, BuildingId parcelId, 1) :: events, added + 1
                else
                    world, events, added
            | _ -> world, events, added

        let world, events, _ =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.fold folder (world, [], 0)

        world, List.rev events

    let private generatedJobsAtPlace placeId world =
        world.GeneratedJobs
        |> Map.toSeq
        |> Seq.filter (fun (_, job) -> job.Place = placeId)
        |> Seq.length

    let private wageFor useKind =
        match useKind with
        | Commerce -> 145m
        | Industry -> 175m
        | _ -> 150m

    let private materializeJobs (world: World) =
        let maxJobsPerTick = 4

        let folder (world, events, added) (parcelId, (parcel: Parcel)) =
            match parcel.Building with
            | Some building
                when (building.Use = Commerce || building.Use = Industry)
                     && building.Status = Occupied
                     && parcel.RoadConnected
                     && parcel.Powered
                     && (if building.Use = Commerce then world.City.Demand.Commercial > 0.10 else world.City.Demand.Industrial > 0.10)
                     && added < maxJobsPerTick ->
                let worldWithPlace, placeId = placeForParcel world parcelId parcel building
                let existingJobs = generatedJobsAtPlace placeId worldWithPlace
                let targetJobs = min building.Jobs (existingJobs + (maxJobsPerTick - added))

                if existingJobs < targetJobs then
                    let newJobs =
                        [ existingJobs .. targetJobs - 1 ]
                        |> List.map (fun index ->
                            let jobId = JobId(guid worldWithPlace "development-job" (parcelId.GetHashCode() + index))
                            jobId,
                            { Id = jobId
                              Employer = None
                              Place = placeId
                              Kind = if building.Use = Industry then "organic industrial job" else "organic commercial job"
                              WagePerDay = wageFor building.Use
                              RequiredSkill = None
                              StartMinute = if building.Use = Industry then 7 * 60 + 30 else 9 * 60
                              EndMinute = if building.Use = Industry then 15 * 60 + 30 else 17 * 60
                              Stability = clamp01 (parcel.Desirability + 0.35)
                              CommuteSensitivity = 0.60 })

                    let world = { worldWithPlace with GeneratedJobs = (worldWithPlace.GeneratedJobs, newJobs) ||> List.fold (fun jobs (jobId, job) -> Map.add jobId job jobs) }
                    let events = JobsCreated(eventId world "jobs-created-organic" added, SimulationSystemCommand EconomySystem, newJobs |> List.map fst) :: events

                    let events =
                        if existingJobs = 0 then
                            BusinessOpened(eventId world "business-opened-organic" added, placeId) :: events
                        else
                            events

                    world, events, added + newJobs.Length
                else
                    worldWithPlace, events, added
            | _ -> world, events, added

        let world, events, _ =
            world.City.Parcels
            |> Map.toSeq
            |> Seq.fold folder (world, [], 0)

        world, List.rev events

    let private vacantUnits world =
        world.HousingUnits
        |> Map.toSeq
        |> Seq.filter (fun (_, unit) -> unit.Vacancy && unit.Occupants.IsEmpty && unit.Habitability >= 0.45)
        |> Seq.sortBy (fun (_, unit) -> unit.RentMonthly |> Option.defaultValue 0m, unit.Id)
        |> Seq.toList

    let private openJob world =
        let filledCounts =
            world.Sims
            |> Map.toSeq
            |> Seq.choose (fun (_, sim) -> sim.Job |> Option.map (fun job -> job.Workplace, job.Title))
            |> Seq.countBy id
            |> Map.ofSeq

        let _, candidate =
            world.GeneratedJobs
            |> Map.toSeq
            |> Seq.sortBy fst
            |> Seq.fold
                (fun (seen, candidate) (_, job) ->
                    match candidate with
                    | Some _ -> seen, candidate
                    | None ->
                        let key = job.Place, job.Kind
                        let used = filledCounts |> Map.tryFind key |> Option.defaultValue 0
                        let seenForKey = seen |> Map.tryFind key |> Option.defaultValue 0
                        let seen = Map.add key (seenForKey + 1) seen

                        if seenForKey >= used then
                            seen, Some job
                        else
                            seen, None)
                (Map.empty, None)

        candidate

    let private nearestResidenceInNeighborhood world neighborhoodId =
        let householdHomes =
            world.HousingUnits
            |> Map.toSeq
            |> Seq.filter (fun (_, unit) -> unit.Neighborhood = neighborhoodId)
            |> Seq.collect (fun (_, unit) -> unit.Occupants)
            |> Seq.choose (fun householdId -> world.Households |> Map.tryFind householdId |> Option.map _.Home)
            |> Seq.tryHead

        householdHomes
        |> Option.orElseWith (fun () ->
            world.Map.Places
            |> Map.toSeq
            |> Seq.tryFind (fun (_, place) -> place.Kind = Residence)
            |> Option.map fst)

    let private defaultNeeds =
        [ Hunger, { Value = 0.82; DecayPerHour = 0.055; CriticalBelow = 0.30 }
          Energy, { Value = 0.78; DecayPerHour = 0.045; CriticalBelow = 0.25 }
          Social, { Value = 0.62; DecayPerHour = 0.025; CriticalBelow = 0.25 }
          Hygiene, { Value = 0.76; DecayPerHour = 0.030; CriticalBelow = 0.25 }
          Fun, { Value = 0.58; DecayPerHour = 0.030; CriticalBelow = 0.25 }
          Bladder, { Value = 0.88; DecayPerHour = 0.070; CriticalBelow = 0.22 }
          Safety, { Value = 0.88; DecayPerHour = 0.006; CriticalBelow = 0.35 }
          Purpose, { Value = 0.66; DecayPerHour = 0.015; CriticalBelow = 0.28 }
          Learning, { Value = 0.50; DecayPerHour = 0.020; CriticalBelow = 0.25 }
          Comfort, { Value = 0.62; DecayPerHour = 0.018; CriticalBelow = 0.24 }
          Environment, { Value = 0.58; DecayPerHour = 0.015; CriticalBelow = 0.25 } ]
        |> Map.ofList

    let private starterObjects =
        []

    let private migrateHousehold world =
        if world.MinuteOfDay <> 8 * 60 || world.City.Demand.Residential <= 0.25 || CityMetrics.deriveOpenJobs world <= 0 then
            world, []
        else
            match vacantUnits world, openJob world with
            | (unitId, unit) :: _, Some job ->
                match nearestResidenceInNeighborhood world unit.Neighborhood with
                | None -> world, []
                | Some home ->
                    let householdId = HouseholdId(guid world "migrant-household" world.Households.Count)
                    let simId = SimId(guid world "migrant-sim" world.Sims.Count)
                    let rent = unit.RentMonthly |> Option.defaultValue 1000m
                    let household =
                        { Id = householdId
                          Name = $"New Juniper Household %d{world.Households.Count + 1}"
                          Home = home
                          Members = Set.singleton simId
                          Funds = 2600m
                          MonthlyIncome = job.WagePerDay * 21m
                          MonthlyExpenses = rent + 850m
                          RentMonthly = Some rent
                          Debt = 0m
                          Assets = 3000m
                          Benefits = 0m
                          HousingStatus = Rents
                          CareObligations = Set.empty
                          ChoresBacklog = 0.10
                          FoodSecurity = 0.72
                          TransportationAccess = 0.58
                          Stability = 0.64
                          ConflictLevel = 0.08
                          SharedMemories = []
                          SharedGoals = [ "settle into Juniper"; "keep steady work" ]
                          Objects = starterObjects
                          BillsDue = 0m
                          LastBilledWeek = None
                          Cleanliness = 0.70
                          LotValue = 0m }

                    let sim =
                        { Id = simId
                          Name = $"New Resident %d{world.Sims.Count + 1}"
                          LifeStage = Adult
                          Household = householdId
                          Home = home
                          Job =
                            Some
                                { Title = job.Kind
                                  Workplace = job.Place
                                  StartMinute = job.StartMinute
                                  EndMinute = job.EndMinute
                                  PayPerDay = job.WagePerDay }
                          School = None
                          AgeDays = 11000 + world.Sims.Count
                          Traits = [ Ambitious ]
                          Skills = Map.empty
                          Emotion = Fine
                          Moodlets = []
                          Aspiration = Some { Kind = CareerSuccess; Progress = 0.05; RewardPoints = 0 }
                          Fears = [ FearOfPoverty ]
                          ActionQueue = []
                          Memories = []
                          SocialCapacity = 7
                          Needs = defaultNeeds
                          Personality =
                            { Openness = 0.55
                              Conscientiousness = 0.62
                              Extraversion = 0.50
                              Agreeableness = 0.60
                              Neuroticism = 0.35
                              Ambition = 0.58
                              Frugality = 0.56
                              RoutinePreference = 0.55 }
                          Location = AtPlace home
                          Activity = Idle
                          Wallet = 450m
                          Happiness = 0.64
                          Guardians = []
                          Dependents = []
                          Relationships = Map.empty
                          HouseholdInventory = Map.empty
                          Wants = [] }

                    let world =
                        { world with
                            Sims = Map.add simId sim world.Sims
                            Households = Map.add householdId household world.Households
                            HousingUnits =
                                world.HousingUnits
                                |> Map.add unitId { unit with Occupants = Set.singleton householdId; Vacancy = false }
                            Neighborhoods =
                                world.Neighborhoods
                                |> Map.change unit.Neighborhood (Option.map (fun n ->
                                    { n with
                                        Residents = Set.add householdId n.Residents
                                        VacancyRate = clamp01 (n.VacancyRate - 0.02) })) }

                    let events =
                        [ HouseholdCreated(eventId world "household-created" world.Households.Count, householdId, [ simId ])
                          HouseholdMovedIn(eventId world "household-moved-in" world.Households.Count, householdId, unitId)
                          JobStarted(eventId world "migrant-job-started" world.Households.Count, simId, job.Place) ]

                    world, events
            | _ -> world, []

    let tick world =
        let world, housingEvents = materializeHousing world
        let world, jobEvents = materializeJobs world
        let world, migrationEvents = migrateHousehold world
        world, housingEvents @ jobEvents @ migrationEvents
