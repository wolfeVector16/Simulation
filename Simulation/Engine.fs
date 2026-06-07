namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module Engine =
    let private score value =
        match Quantities.score (clamp01 value) with
        | Ok score -> score
        | Error message -> invalidArg "value" message

    let private deterministicGuid (TickId tick) (index: int) =
        let bytes = Array.zeroCreate<byte> 16
        BitConverter.GetBytes(tick).CopyTo(bytes, 0)
        BitConverter.GetBytes(index).CopyTo(bytes, 4)
        Guid(bytes)

    let private deterministicEventId tick index =
        EventId(deterministicGuid tick (index + 10_000))

    let private householdBudgetPressure household =
        if household.BillsDue <= 0m then
            None
        else
            let monthlyIncome = max 1m household.MonthlyIncome
            let burden = decimal household.BillsDue / monthlyIncome |> float
            Some(clamp01 burden)

    let createSnapshot world =
        WorldSnapshot.create
            { Tick = TickId world.Meta.Tick
              Time = { Day = world.Day; MinuteOfDay = world.MinuteOfDay }
              SimIds = world.Sims |> Map.toSeq |> Seq.map fst |> Seq.sort |> Seq.toArray
              HouseholdIds = world.Households |> Map.toSeq |> Seq.map fst |> Seq.sort |> Seq.toArray
              NeighborhoodIds = world.Neighborhoods |> Map.toSeq |> Seq.map fst |> Seq.sort |> Seq.toArray
              InstitutionIds = world.Institutions |> Map.toSeq |> Seq.map fst |> Seq.sort |> Seq.toArray
              ActiveTripIds =
                world.Transport.Trips
                |> Map.toSeq
                |> Seq.choose (fun (tripId, trip) -> if trip.Status = InProgress then Some tripId else None)
                |> Seq.sort
                |> Seq.toArray
              LaneIds = world.Transport.Lanes |> Map.toSeq |> Seq.map fst |> Seq.sort |> Seq.toArray }

    let generatePressures snapshot world =
        let data = WorldSnapshot.value snapshot

        let pressures =
            world.Households
            |> Map.toSeq
            |> Seq.choose (fun (householdId, household) ->
                householdBudgetPressure household
                |> Option.map (fun magnitude ->
                    { Entity = HouseholdEntity householdId
                      Kind = "household_budget"
                      Magnitude = score magnitude
                      Reason = Some FinancialPressure }))
            |> Seq.toArray

        { Tick = data.Tick
          Partition = PartitionId "world"
          Pressures = pressures }

    let generateIntents pressureBatch world =
        let intents =
            pressureBatch.Pressures
            |> Array.choose (fun pressure ->
                match pressure.Entity with
                | HouseholdEntity householdId ->
                    world.Households
                    |> Map.tryFind householdId
                    |> Option.map (fun household ->
                        let amount = household.BillsDue
                        let chosenAction = if household.Funds >= amount then PayBillAction amount else DelayBillAction amount
                        { Id = deterministicGuid pressureBatch.Tick world.Meta.Decisions.Length
                          PartitionKey = sprintf "%A" householdId
                          Decision =
                            { Actor = None
                              Household = Some householdId
                              ChosenAction = chosenAction
                              RejectedAlternatives = []
                              Reasons = pressure.Reason |> Option.toList
                              ExpectedConsequences = []
                              Confidence = 0.75
                              Urgency = Quantities.scoreValue pressure.Magnitude
                              TimeCostMinutes = 0
                              MoneyCost = amount
                              SocialCost = 0.0
                              Risk = 1.0 - household.Stability } })
                | _ -> None)
            |> Array.mapi (fun index intent -> { intent with Id = deterministicGuid pressureBatch.Tick index })

        { Tick = pressureBatch.Tick
          Partition = pressureBatch.Partition
          Intents = intents }

    let resolveConflicts intentBatch _world =
        let resolved =
            intentBatch.Intents
            |> Array.sortBy (fun intent -> intent.PartitionKey, intent.Id)
            |> Array.mapi (fun index intent ->
                { Intent = intent
                  ResolutionRank = index
                  TieBreaker = intent.Id })

        { Tick = intentBatch.Tick
          Partition = intentBatch.Partition
          Resolved = resolved }

    let emitEvents resolvedBatch _world : EventBatch =
        let events =
            resolvedBatch.Resolved
            |> Array.choose (fun resolved ->
                match resolved.Intent.Decision.Household, resolved.Intent.Decision.ChosenAction with
                | Some householdId, PayBillAction amount ->
                    Some(BillPaid(deterministicEventId resolvedBatch.Tick resolved.ResolutionRank, householdId, amount))
                | Some householdId, DelayBillAction amount ->
                    Some(BillMissed(deterministicEventId resolvedBatch.Tick resolved.ResolutionRank, householdId, amount))
                | _ -> None)

        { Tick = resolvedBatch.Tick
          Partition = resolvedBatch.Partition
          Events = events
          OrderingRule = "Stable partition key, then deterministic intent id." }

    let applyEvents (eventBatch: EventBatch) world =
        let events = eventBatch.Events |> Array.toList
        let world = SimulationPipeline.applyEvents events world

        { world with
            Meta = { world.Meta with EventLog = (events @ world.Meta.EventLog) |> List.truncate 500 } }

    let updateIndexes world =
        let indexes = SimulationPipeline.rebuildIndexes world
        let runtime = SimulationPipeline.rebuildRuntimeIndexes world

        { world with
            Meta = { world.Meta with Indexes = indexes }
            Runtime = runtime }

    let private changedKeys before after =
        after
        |> Map.toSeq
        |> Seq.choose (fun (key, value) ->
            match before |> Map.tryFind key with
            | Some previous when previous = value -> None
            | _ -> Some key)
        |> Seq.toArray

    let private createChanges before after =
        let roadSegmentsChanged =
            after.Map.RoadSegments
            |> List.filter (fun segment ->
                before.Map.RoadSegments
                |> List.tryFind (fun previous -> previous.Id = segment.Id)
                |> Option.exists ((<>) segment))
            |> List.map _.Id
            |> List.toArray

        { ChangedPeople = changedKeys before.Sims after.Sims
          ChangedHouseholds = changedKeys before.Households after.Households
          ChangedNeighborhoods = changedKeys before.Neighborhoods after.Neighborhoods
          ChangedInstitutions = changedKeys before.Institutions after.Institutions
          ChangedTrips = changedKeys before.Transport.Trips after.Transport.Trips
          ChangedRoadSegments = roadSegmentsChanged
          ChangedLanes = changedKeys before.Transport.Lanes after.Transport.Lanes
          ChangedRelationships = changedKeys before.Relationships after.Relationships }

    let private emittedEvents before after =
        let newEventCount = max 0 (after.Meta.EventLog.Length - before.Meta.EventLog.Length)
        after.Meta.EventLog |> List.truncate newEventCount |> List.toArray

    let private logCommandEvents events world =
        if List.isEmpty events then
            world
        else
            { world with
                Meta = { world.Meta with EventLog = (events @ world.Meta.EventLog) |> List.truncate 500 } }

    let private createTickResult before after =
        let tick = TickId before.Meta.Tick
        let events = emittedEvents before after

        { Tick = tick
          Events =
            { Tick = tick
              Partition = PartitionId "world"
              Events = events
              OrderingRule = "Events are appended by deterministic simulation phases." }
          Changes = createChanges before after
          NarrativeSummaries = [| sprintf "Processed %i events." events.Length |]
          Diagnostics = after.PerformanceDiagnostics.PhaseDiagnostics |> List.toArray }

    let private advanceWorld minutes world =
        let currentMinute = world.MinuteOfDay
        let nextAbsoluteMinute = world.MinuteOfDay + minutes
        let nextMinute = normalizeMinute nextAbsoluteMinute
        let nextDay = world.Day + (nextAbsoluteMinute / minutesPerDay)
        let cityMap = Economy.tickPlaces minutes world.Map
        let city = CitySystems.tick minutes world.City

        let cityMap, sims =
            ((cityMap, Map.empty), world.Sims |> Map.toSeq)
            ||> Seq.fold (fun (cityMap, sims) (simId, sim) ->
                let cityMap, sim = SimBehavior.updateSim cityMap world.Households world.Day currentMinute minutes sim
                cityMap, Map.add simId sim sims)

        { world with
            Day = nextDay
            MinuteOfDay = nextMinute
            Map = cityMap
            City = city
            Sims = sims }
        |> Transport.tick minutes
        |> LifeSim.tick minutes
        |> SimulationPipeline.tick

    let advanceTickWithAuthority authorityMode minutes (input: TickInput) world : World * TickResult =
        let snapshot = createSnapshot world
        let commandResult = CommandSystem.executeCommandBatch authorityMode world input.Commands
        let pressures = generatePressures snapshot commandResult.World
        let intents = generateIntents pressures commandResult.World
        let resolved = resolveConflicts intents commandResult.World
        let events = emitEvents resolved commandResult.World

        let next =
            commandResult.World
            |> applyEvents events
            |> advanceWorld minutes
            |> logCommandEvents commandResult.Events
            |> updateIndexes
            |> fun world -> { world with Diagnostics = Diagnostics.tick world }

        next, createTickResult world next

    let advanceTick minutes input world : World * TickResult =
        advanceTickWithAuthority MayorMode minutes input world

    let tickWithResult minutes world : World * TickResult =
        advanceTick minutes { Tick = TickId world.Meta.Tick; Commands = [] } world

    let tick minutes world =
        tickWithResult minutes world |> fst
