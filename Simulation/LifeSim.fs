namespace Simulation

open Simulation.Domain
open Simulation.Measures

module LifeSim =
    let private skillXpPerLevel = 100.0

    let private needValue kind (sim: Sim) =
        sim.Needs
        |> Map.tryFind kind
        |> Option.map _.Value
        |> Option.defaultValue 0.5

    let private updateNeed kind amount (sim: Sim) =
        let needs =
            sim.Needs
            |> Map.change kind (Option.map (fun need -> { need with Value = clamp01 (need.Value + amount) }))

        { sim with Needs = needs }

    let private usable kind (household: Household) =
        household.Objects
        |> List.filter (fun item -> item.Kind = kind && not item.Broken)
        |> List.sortByDescending (fun item -> item.Quality)
        |> List.tryHead

    let private objectWithInteraction interaction (household: Household) =
        household.Objects
        |> List.filter (fun item -> not item.Broken && List.contains interaction item.Interactions)
        |> List.sortByDescending (fun item -> item.Quality)
        |> List.tryHead

    let private lowestNeed (sim: Sim) =
        sim.Needs
        |> Map.toSeq
        |> Seq.minBy (fun (_, need) -> need.Value)

    let private actionForQueued household (action: QueuedAction) =
        match action.Target with
        | Some objectId ->
            household.Objects
            |> List.tryFind (fun item -> item.Id = objectId && not item.Broken)
            |> Option.map (fun _ -> UsingObject action.Interaction)
        | None ->
            Some(UsingObject action.Interaction)

    let suggestHomeActivity (sim: Sim) (household: Household) =
        let queueChoice =
            sim.ActionQueue
            |> List.sortByDescending _.Priority
            |> List.tryPick (actionForQueued household)

        match queueChoice with
        | Some activity -> Some activity
        | None ->
            let kind, need = lowestNeed sim

            if need.Value < 0.42 then
                match kind with
                | Hunger ->
                    objectWithInteraction CookMeal household
                    |> Option.orElseWith (fun () -> objectWithInteraction GrabSnack household)
                    |> Option.map (fun item ->
                        if List.contains CookMeal item.Interactions then UsingObject CookMeal else UsingObject GrabSnack)
                | Energy ->
                    usable BedObject household |> Option.map (fun _ -> UsingObject SleepInBed)
                | Bladder ->
                    usable ToiletObject household |> Option.map (fun _ -> UsingObject UseToilet)
                | Hygiene ->
                    usable ShowerObject household |> Option.map (fun _ -> UsingObject ShowerSelf)
                | Fun ->
                    objectWithInteraction PlayWithToys household
                    |> Option.map (fun _ -> UsingObject PlayWithToys)
                    |> Option.orElseWith (fun () -> objectWithInteraction PlayGames household |> Option.map (fun _ -> UsingObject PlayGames))
                    |> Option.orElseWith (fun () -> objectWithInteraction WatchTv household |> Option.map (fun _ -> UsingObject WatchTv))
                | Comfort ->
                    usable SofaObject household
                    |> Option.orElseWith (fun () -> usable BedObject household)
                    |> Option.map (fun _ -> Relaxing)
                | Environment ->
                    if household.Cleanliness < 0.70 then Some Cleaning else None
                | Learning ->
                    objectWithInteraction ReadBook household
                    |> Option.map (fun _ -> UsingObject ReadBook)
                    |> Option.orElseWith (fun () -> Some Studying)
                | Social
                | Safety
                | Purpose -> None
            elif List.contains Neat sim.Traits && household.Cleanliness < 0.75 then
                Some Cleaning
            elif List.contains Creative sim.Traits then
                objectWithInteraction (PracticeSkill Painting) household
                |> Option.map (fun _ -> PracticingSkill Painting)
            elif List.contains Genius sim.Traits then
                objectWithInteraction (PracticeSkill Logic) household
                |> Option.map (fun _ -> PracticingSkill Logic)
            else
                None

    let private addMoodlet (moodlet: Moodlet) (sim: Sim) =
        let moodlets =
            sim.Moodlets
            |> List.filter (fun existing -> existing.Name <> moodlet.Name)

        { sim with Moodlets = moodlet :: moodlets }

    let private needMoodlets (sim: Sim) =
        [ if needValue Hunger sim < 0.25 then
              { Name = "Ravenous"
                Emotion = Tense
                Strength = 0.55
                RemainingMinutes = 90 }
          if needValue Energy sim < 0.22 then
              { Name = "Exhausted"
                Emotion = Tense
                Strength = 0.50
                RemainingMinutes = 120 }
          if needValue Hygiene sim < 0.25 then
              { Name = "Grimy"
                Emotion = Embarrassed
                Strength = 0.35
                RemainingMinutes = 90 }
          if needValue Social sim < 0.25 then
              { Name = "Lonely"
                Emotion = Sad
                Strength = 0.35
                RemainingMinutes = 120 }
          if needValue Fun sim < 0.25 then
              { Name = "Bored"
                Emotion = Tense
                Strength = 0.30
                RemainingMinutes = 90 }
          if needValue Environment sim > 0.75 then
              { Name = "Pleasant surroundings"
                Emotion = Happy
                Strength = 0.20
                RemainingMinutes = 120 } ]

    let private activityMoodlets activity =
        match activity with
        | UsingObject CookMeal ->
            [ { Name = "Good meal"
                Emotion = Happy
                Strength = 0.25
                RemainingMinutes = 120 } ]
        | UsingObject ShowerSelf ->
            [ { Name = "Fresh and clean"
                Emotion = Happy
                Strength = 0.20
                RemainingMinutes = 90 } ]
        | PracticingSkill Painting ->
            [ { Name = "Creative flow"
                Emotion = Inspired
                Strength = 0.25
                RemainingMinutes = 120 } ]
        | PracticingSkill Logic
        | Studying
        | UsingObject ReadBook ->
            [ { Name = "Focused mind"
                Emotion = Focused
                Strength = 0.22
                RemainingMinutes = 90 } ]
        | Socializing ->
            [ { Name = "Good conversation"
                Emotion = Happy
                Strength = 0.18
                RemainingMinutes = 90 } ]
        | Cleaning ->
            [ { Name = "Productive chores"
                Emotion = Focused
                Strength = 0.12
                RemainingMinutes = 60 } ]
        | _ -> []

    let private decayMoodlets minutes (sim: Sim) =
        { sim with
            Moodlets =
                sim.Moodlets
                |> List.choose (fun moodlet ->
                    let remaining = moodlet.RemainingMinutes - minutes
                    if remaining > 0 then Some { moodlet with RemainingMinutes = remaining } else None) }

    let private chooseEmotion happiness (moodlets: Moodlet list) =
        let strongest =
            moodlets
            |> List.sortByDescending (fun (moodlet: Moodlet) -> moodlet.Strength)
            |> List.tryHead

        match strongest with
        | Some moodlet when moodlet.Strength >= 0.18 -> moodlet.Emotion
        | _ when happiness > 0.70 -> Happy
        | _ when happiness < 0.30 -> Sad
        | _ -> Fine

    let private improveSkill skillKind xp sim =
        let current =
            sim.Skills
            |> Map.tryFind skillKind
            |> Option.defaultValue { Level = 0; Experience = 0.0 }

        let totalXp = current.Experience + xp
        let gainedLevels = int (totalXp / skillXpPerLevel)
        let level = min 10 (current.Level + gainedLevels)
        let experience = if level >= 10 then skillXpPerLevel else totalXp % skillXpPerLevel

        { sim with Skills = Map.add skillKind { Level = level; Experience = experience } sim.Skills }

    let private skillsFromActivity minutes (sim: Sim) =
        let xp = float minutes * 0.9

        match sim.Activity with
        | Working ->
            match sim.Job with
            | Some job when job.Title.Contains("Analyst") -> improveSkill Logic (xp * 0.55) sim
            | Some job when job.Title.Contains("Fabricator") -> improveSkill Handiness (xp * 0.65) sim
            | _ -> sim
        | AttendingSchool
        | Studying
        | UsingObject ReadBook -> improveSkill Logic (xp * 0.45) sim
        | UsingObject CookMeal -> improveSkill Cooking xp sim
        | UsingObject PlayGames -> improveSkill Programming (xp * 0.35) sim
        | UsingObject PlayWithToys -> improveSkill Creativity (xp * 0.25) sim
        | PracticingSkill skill -> improveSkill skill xp sim
        | Socializing -> improveSkill Charisma (xp * 0.25) sim
        | Repairing
        | UsingObject RepairObject -> improveSkill Handiness xp sim
        | _ -> sim

    let private updateAspiration (sim: Sim) =
        match sim.Aspiration with
        | None -> sim
        | Some aspiration ->
            let progressGain =
                match aspiration.Kind, sim.Activity with
                | CareerSuccess, Working -> 0.010
                | BigHappyFamily, CaringForChild _ -> 0.015
                | BigHappyFamily, Socializing when not (List.isEmpty sim.Dependents) -> 0.008
                | KnowledgeSeeker, Studying
                | KnowledgeSeeker, AttendingSchool
                | KnowledgeSeeker, PracticingSkill Logic -> 0.012
                | SocialButterfly, Socializing -> 0.012
                | CreativeLife, PracticingSkill Painting
                | CreativeLife, PracticingSkill Writing
                | CreativeLife, PracticingSkill Music -> 0.014
                | WealthBuilder, Shopping(WantPurchase, LuxuryGoods) -> 0.006
                | _ -> 0.0

            let progress = clamp01 (aspiration.Progress + progressGain)
            let rewardPoints =
                if progress >= 1.0 && aspiration.Progress < 1.0 then
                    aspiration.RewardPoints + 500
                else
                    aspiration.RewardPoints

            { sim with Aspiration = Some { aspiration with Progress = progress; RewardPoints = rewardPoints } }

    let private applyObjectEffects (household: Household) minutes (sim: Sim) =
        match sim.Location with
        | AtPlace place when place = household.Home ->
            let averageQuality =
                match household.Objects with
                | [] -> 0.4
                | objects -> objects |> List.averageBy _.Quality

            let decor =
                household.Objects
                |> List.filter (fun item -> item.Kind = DecorObject)
                |> function
                    | [] -> 0.0
                    | objects -> objects |> List.averageBy _.Quality

            let hours = float minutes / 60.0

            sim
            |> updateNeed Comfort ((averageQuality - 0.45) * 0.08 * hours)
            |> updateNeed Environment ((household.Cleanliness - 0.45 + decor * 0.20) * 0.08 * hours)
        | _ -> sim

    let private applyActivityNeedEffects minutes (sim: Sim) =
        let hours = float minutes / 60.0

        match sim.Activity with
        | UsingObject SleepInBed ->
            sim |> updateNeed Energy (0.40 * hours) |> updateNeed Comfort (0.12 * hours)
        | UsingObject CookMeal ->
            sim |> updateNeed Hunger (0.58 * hours) |> updateNeed Fun (0.04 * hours)
        | UsingObject GrabSnack ->
            sim |> updateNeed Hunger (0.35 * hours)
        | UsingObject ShowerSelf ->
            sim |> updateNeed Hygiene (0.55 * hours)
        | UsingObject UseToilet ->
            sim |> updateNeed Bladder (0.65 * hours)
        | UsingObject WatchTv
        | UsingObject PlayGames
        | UsingObject PlayWithToys ->
            sim |> updateNeed Fun (0.32 * hours) |> updateNeed Comfort (0.04 * hours)
        | UsingObject ReadBook ->
            sim |> updateNeed Learning (0.20 * hours) |> updateNeed Fun (0.06 * hours)
        | Cleaning ->
            sim |> updateNeed Environment (0.20 * hours)
        | Repairing ->
            sim |> updateNeed Purpose (0.16 * hours)
        | PracticingSkill _ ->
            sim |> updateNeed Purpose (0.12 * hours) |> updateNeed Fun (0.05 * hours)
        | _ -> sim

    let private updateRelationships (allSims: Map<SimId, Sim>) (sim: Sim) =
        match sim.Activity with
        | Socializing ->
            let householdMates =
                allSims
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.filter (fun (other: Sim) -> other.Id <> sim.Id && other.Household = sim.Household)
                |> Seq.toList

            let relationships =
                (sim.Relationships, householdMates)
                ||> List.fold (fun state (other: Sim) ->
                    let current = Map.tryFind other.Id state |> Option.defaultValue 0.0
                    Map.add other.Id (clamp01 (current + 0.015)) state)

            { sim with Relationships = relationships }
        | CaringForChild childId ->
            let current = Map.tryFind childId sim.Relationships |> Option.defaultValue 0.0
            { sim with Relationships = Map.add childId (clamp01 (current + 0.020)) sim.Relationships }
        | _ -> sim

    let private updateAge minutes (sim: Sim) =
        if minutes <= 0 then
            sim
        else
            sim

    let tickHouseholds minutes (households: Map<HouseholdId, Household>) =
        let hours = float minutes / 60.0

        households
        |> Map.map (fun _ (household: Household) ->
            let cleaningDelta = -0.006 * hours * max 1.0 (float household.Objects.Length / 8.0)
            let repairDrag =
                household.Objects
                |> List.filter (fun obj -> obj.Broken)
                |> List.length
                |> float
                |> (*) 0.01

            { household with
                Cleanliness = clamp01 (household.Cleanliness + cleaningDelta - repairDrag) })

    let tickSim minutes (households: Map<HouseholdId, Household>) (allSims: Map<SimId, Sim>) (sim: Sim) =
        let household = Map.tryFind sim.Household households

        let sim =
            sim
            |> decayMoodlets minutes
            |> applyActivityNeedEffects minutes
            |> skillsFromActivity minutes
            |> updateRelationships allSims
            |> updateAspiration
            |> updateAge minutes

        let sim =
            match household with
            | Some household -> applyObjectEffects household minutes sim
            | None -> sim

        let sim =
            (sim, needMoodlets sim @ activityMoodlets sim.Activity)
            ||> List.fold (fun sim moodlet -> addMoodlet moodlet sim)

        { sim with Emotion = chooseEmotion sim.Happiness sim.Moodlets }

    let tick minutes world =
        let households = tickHouseholds minutes world.Households
        let sims = world.Sims |> Map.map (fun _ sim -> tickSim minutes households world.Sims sim)
        { world with Households = households; Sims = sims }
