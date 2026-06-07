namespace Simulation

open Simulation.Domain
open Simulation.Measures

module SimBehavior =

    let private updateNeed minutes activity kind need =
        let hours = float minutes / 60.0

        let activityEffect =
            match activity, kind with
            | Sleeping, Energy -> 0.34 * hours
            | Sleeping, Fun -> -0.01 * hours
            | Working, Purpose -> 0.06 * hours
            | Working, Fun -> -0.08 * hours
            | Working, Hunger -> -0.04 * hours
            | Eating, Hunger -> 0.55 * hours
            | Eating, Social -> 0.05 * hours
            | Relaxing, Fun -> 0.22 * hours
            | Relaxing, Energy -> 0.05 * hours
            | Socializing, Social -> 0.35 * hours
            | Socializing, Fun -> 0.10 * hours
            | AttendingSchool, Learning -> 0.28 * hours
            | AttendingSchool, Fun -> -0.03 * hours
            | AttendingSchool, Energy -> -0.02 * hours
            | InDaycare, Safety -> 0.12 * hours
            | InDaycare, Social -> 0.12 * hours
            | Playing, Fun -> 0.30 * hours
            | Playing, Social -> 0.08 * hours
            | Studying, Learning -> 0.22 * hours
            | Studying, Fun -> -0.04 * hours
            | CaringForChild _, Social -> 0.08 * hours
            | CaringForChild _, Purpose -> 0.05 * hours
            | MorningRoutine, Hygiene -> 0.40 * hours
            | MorningRoutine, Bladder -> 0.35 * hours
            | Commuting _, Fun -> -0.03 * hours
            | Commuting _, Energy -> -0.02 * hours
            | Errand, Purpose -> 0.04 * hours
            | _ -> 0.0

        let passiveDecay = need.DecayPerHour * hours
        { need with Value = clamp01 (need.Value - passiveDecay + activityEffect) }

    let private updateNeeds minutes activity needs =
        needs
        |> Map.map (fun kind need -> updateNeed minutes activity kind need)

    let private lowestNeed needs =
        needs
        |> Map.toSeq
        |> Seq.minBy (fun (_, need) -> need.Value)

    let private needPressure needs =
        needs
        |> Map.toSeq
        |> Seq.averageBy (fun (_, need) ->
            if need.Value < need.CriticalBelow then
                1.0 - need.Value
            else
                (1.0 - need.Value) * 0.25)

    let private isBetween startMinute endMinute minute =
        if startMinute <= endMinute then
            minute >= startMinute && minute < endMinute
        else
            minute >= startMinute || minute < endMinute

    let private minutesUntil target current =
        let target = normalizeMinute target
        let current = normalizeMinute current
        if target >= current then target - current else minutesPerDay - current + target

    let private startTrip purpose origin destination minutes sim =
        { sim with
            Location =
                InTransit
                    { Origin = origin
                      Destination = destination
                      Purpose = purpose
                      RemainingMinutes = minutes
                      TotalMinutes = minutes }
            Activity = Commuting purpose }

    let private commuteMinutes (cityMap: CityMap) origin destination =
        MapGraph.routeMinutes cityMap origin destination
        |> Option.defaultValue 35

    let private inventoryOf good (sim: Sim) =
        sim.HouseholdInventory
        |> Map.tryFind good
        |> Option.defaultValue 0.0

    let private isWeekend day =
        day % 7 = 6 || day % 7 = 0

    let private isSchoolDay day = not (isWeekend day)

    let private canShopAlone (sim: Sim) =
        match sim.LifeStage with
        | Teen
        | YoungAdult
        | Adult
        | Elder -> true
        | Infant
        | Toddler
        | Child -> false

    let private needShoppingRequest (sim: Sim) =
        let hunger = sim.Needs[Hunger].Value
        let hygiene = sim.Needs[Hygiene].Value

        if hunger < 0.42 && inventoryOf Groceries sim < 1.0 then
            Some(Groceries, NeedPurchase)
        elif hygiene < 0.35 && inventoryOf HouseholdGoods sim < 1.0 then
            Some(HouseholdGoods, NeedPurchase)
        else
            None

    let private wantShoppingRequest day minute (sim: Sim) =
        if not (canShopAlone sim) || not (isWeekend day) || minute < 10 * 60 || minute > 18 * 60 then
            None
        else
            sim.Wants
            |> List.filter (fun want -> (not want.WeekendOnly || isWeekend day) && inventoryOf want.Good sim < 1.0)
            |> List.sortByDescending (fun want -> want.Desire)
            |> List.tryHead
            |> Option.bind (fun want ->
                let funNeed = sim.Needs[Fun].Value
                let desirePressure = want.Desire + (1.0 - funNeed) * 0.35 + sim.Personality.Openness * 0.15

                if desirePressure > 0.75 then
                    Some(want.Good, WantPurchase)
                else
                    None)

    let private dueToLeaveFor cityMap origin destination targetMinute currentMinute minutes =
        let leaveBuffer = commuteMinutes cityMap origin destination + 10 + minutes
        minutesUntil targetMinute currentMinute <= leaveBuffer

    let private tryStartShoppingTrip (cityMap: CityMap) intent good origin (sim: Sim) =
        Economy.findShoppingDestination cityMap sim origin good intent
        |> Option.map (fun (destination, minutes) ->
            startTrip (ToShopping(intent, good)) origin destination (max 5 minutes) sim)
        |> Option.defaultValue sim

    let private chooseAtPlaceActivity (households: Map<HouseholdId, Household>) day minute (sim: Sim) =
        let homeAutonomy =
            match sim.Location, Map.tryFind sim.Household households with
            | AtPlace place, Some household when place = sim.Home ->
                LifeSim.suggestHomeActivity sim household
            | _ -> None

        let lowestKind, lowest = lowestNeed sim.Needs

        match homeAutonomy with
        | Some activity when lowest.Value < 0.55 || List.contains Neat sim.Traits ->
            activity
        | _ ->
          if lowest.Value < lowest.CriticalBelow then
            match lowestKind with
            | Hunger -> Eating
            | Energy -> Sleeping
            | Social -> Socializing
            | Hygiene
            | Bladder -> MorningRoutine
            | Fun -> Relaxing
            | Safety -> Idle
            | Purpose -> Errand
            | Learning -> Studying
            | Comfort -> Relaxing
            | Environment -> Cleaning
          else
            match sim.School with
            | Some school when isSchoolDay day && sim.Location = AtPlace school.School && isBetween school.StartMinute school.EndMinute minute ->
                AttendingSchool
            | _ ->
                match sim.Job with
                | Some job when not (isWeekend day) && sim.Location = AtPlace job.Workplace && isBetween job.StartMinute job.EndMinute minute ->
                    Working
                | _ ->
                    match sim.LifeStage with
                    | Infant
                    | Toddler ->
                        if minute >= 20 * 60 || minute < 6 * 60 then Sleeping else Playing
                    | Child
                    | Teen ->
                        if minute >= 21 * 60 || minute < 6 * 60 then
                            Sleeping
                        elif minute < 8 * 60 then
                            MorningRoutine
                        elif minute >= 16 * 60 && minute < 18 * 60 then
                            Studying
                        else
                            Playing
                    | YoungAdult
                    | Adult
                    | Elder ->
                        if minute >= 22 * 60 || minute < 6 * 60 then
                            Sleeping
                        elif minute < 8 * 60 then
                            MorningRoutine
                        elif sim.Personality.Extraversion > 0.65 && minute >= 18 * 60 then
                            Socializing
                        else
                            Relaxing

    let private updateMood (sim: Sim) =
        let averageNeed =
            sim.Needs
            |> Map.toSeq
            |> Seq.averageBy (fun (_, need) -> need.Value)

        let stress = needPressure sim.Needs * (0.5 + sim.Personality.Neuroticism)
        let mood = averageNeed - stress
        { sim with Happiness = clamp01 mood }

    let private advanceTrip minutes (trip: Trip) =
        { trip with RemainingMinutes = max 0 (trip.RemainingMinutes - minutes) }

    let private updateMovement (cityMap: CityMap) day minute minutes (sim: Sim) =
        match sim.Location with
        | InTransit trip ->
            let updatedTrip = advanceTrip minutes trip

            if updatedTrip.RemainingMinutes = 0 then
                let activity =
                    match updatedTrip.Purpose with
                    | ToShopping(intent, good) -> Shopping(intent, good)
                    | ToSchool -> AttendingSchool
                    | ToDaycare -> InDaycare
                    | _ -> Idle

                { sim with
                    Location = AtPlace updatedTrip.Destination
                    Activity = activity }
            else
                { sim with
                    Location = InTransit updatedTrip
                    Activity = Commuting updatedTrip.Purpose }
        | AtPlace place ->
            match sim.Job with
            | _ when isSchoolDay day && place = sim.Home && sim.School.IsSome ->
                let school = sim.School.Value

                if dueToLeaveFor cityMap sim.Home school.School school.StartMinute minute minutes then
                    let purpose = if school.NeedsEscort then ToDaycare else ToSchool
                    startTrip purpose sim.Home school.School (commuteMinutes cityMap sim.Home school.School) sim
                else
                    sim
            | _ when isSchoolDay day && sim.School.IsSome && place = sim.School.Value.School && minute >= sim.School.Value.EndMinute && minute < 20 * 60 ->
                let purpose = if sim.School.Value.NeedsEscort then FromDaycare else FromSchool
                startTrip purpose place sim.Home (commuteMinutes cityMap place sim.Home) sim
            | Some job when not (isWeekend day) && place = sim.Home ->
                let dueToLeave = dueToLeaveFor cityMap sim.Home job.Workplace job.StartMinute minute minutes

                if dueToLeave then
                    startTrip ToWork sim.Home job.Workplace (commuteMinutes cityMap sim.Home job.Workplace) sim
                else
                    match needShoppingRequest sim with
                    | Some (good, intent) -> tryStartShoppingTrip cityMap intent good place sim
                    | None ->
                        match wantShoppingRequest day minute sim with
                        | Some (good, intent) -> tryStartShoppingTrip cityMap intent good place sim
                        | None -> sim
            | _ when place = sim.Home && canShopAlone sim ->
                match needShoppingRequest sim with
                | Some (good, intent) -> tryStartShoppingTrip cityMap intent good place sim
                | None ->
                    match wantShoppingRequest day minute sim with
                    | Some (good, intent) -> tryStartShoppingTrip cityMap intent good place sim
                    | None -> sim
            | Some job when place = job.Workplace && (isWeekend day || minute >= job.EndMinute) && minute < 22 * 60 ->
                startTrip ToHome job.Workplace sim.Home (commuteMinutes cityMap job.Workplace sim.Home) sim
            | _ ->
                match sim.Activity with
                | Shopping(NeedPurchase, _)
                | Shopping(WantPurchase, _) ->
                    startTrip ToHome place sim.Home (commuteMinutes cityMap place sim.Home) sim
                | _ -> sim

    let private chooseActivity (cityMap: CityMap) (households: Map<HouseholdId, Household>) day minute (moved: Sim) =
        match moved.Location with
        | InTransit trip -> Commuting trip.Purpose
        | AtPlace placeId ->
            match cityMap.Places |> Map.tryFind placeId with
            | Some place when place.Kind = Commercial ->
                match moved.Activity with
                | Commuting(ToShopping(intent, good))
                | Shopping(intent, good) -> Shopping(intent, good)
                | _ -> chooseAtPlaceActivity households day minute moved
            | _ -> chooseAtPlaceActivity households day minute moved

    let private applyActivityAtPlace (cityMap: CityMap) activity (sim: Sim) =
        match sim.Location, activity with
        | AtPlace placeId, Shopping(intent, good) ->
            match Map.tryFind placeId cityMap.Places with
            | Some place ->
                let place, sim, bought = Economy.purchaseAt place sim intent good
                let cityMap = { cityMap with Places = Map.add placeId place cityMap.Places }

                if bought then
                    cityMap, sim
                else
                    cityMap, { sim with Activity = Idle }
            | None -> cityMap, sim
        | _ -> cityMap, sim

    let updateSim (cityMap: CityMap) (households: Map<HouseholdId, Household>) day minute minutes (sim: Sim) =
        sim
        |> updateMovement cityMap day minute minutes
        |> fun moved ->
            let nextMinute = normalizeMinute (minute + minutes)
            let activity = chooseActivity cityMap households day nextMinute moved

            let sim =
                { moved with
                    Activity = activity
                    Needs = updateNeeds minutes activity moved.Needs }

            let cityMap, sim = applyActivityAtPlace cityMap activity sim
            cityMap, updateMood sim

