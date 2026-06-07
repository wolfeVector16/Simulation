namespace Simulation

open System
open Simulation.Domain
open Simulation.Measures

module Economy =
    let private unitsForMinutes minutes unitsPerDay =
        unitsPerDay * float minutes / float minutesPerDay

    let private stockOf good inventory =
        Map.tryFind good inventory |> Option.defaultValue 0.0

    let private addStock good units inventory =
        let current = stockOf good inventory
        Map.add good (current + units) inventory

    let private removeStock good units inventory =
        let current = stockOf good inventory
        Map.add good (max 0.0 (current - units)) inventory

    let private updatePlaceEconomy minutes economy =
        let imported =
            economy.ImportsPerDay
            |> Map.fold (fun inventory good unitsPerDay -> addStock good (unitsForMinutes minutes unitsPerDay) inventory) economy.Inventory

        let inventory =
            ((imported, economy.Produces) ||> List.fold (fun inventory recipe ->
                let desiredOutput = unitsForMinutes minutes recipe.UnitsPerDay

                let possibleByInputs =
                    recipe.Inputs
                    |> Map.toSeq
                    |> Seq.map (fun (good, requiredPerUnit) ->
                        if requiredPerUnit <= 0.0 then
                            Double.PositiveInfinity
                        else
                            stockOf good inventory / requiredPerUnit)
                    |> Seq.append [ desiredOutput ]
                    |> Seq.min

                let output = max 0.0 (min desiredOutput possibleByInputs)

                if output <= 0.0 then
                    inventory
                else
                    let consumed =
                        recipe.Inputs
                        |> Map.fold (fun state good requiredPerUnit -> removeStock good (requiredPerUnit * output) state) inventory

                    addStock recipe.Output output consumed))

        { economy with Inventory = inventory }

    let private updatePlace minutes place =
        match place.Economy with
        | Some economy -> { place with Economy = Some(updatePlaceEconomy minutes economy) }
        | None -> place

    let private transfer good units fromPlace toPlace =
        match fromPlace.Economy, toPlace.Economy with
        | Some fromEconomy, Some toEconomy ->
            let moved = min units (stockOf good fromEconomy.Inventory)

            if moved <= 0.0 then
                fromPlace, toPlace, 0.0
            else
                let fromPlace =
                    { fromPlace with
                        Economy = Some { fromEconomy with Inventory = removeStock good moved fromEconomy.Inventory } }

                let toPlace =
                    { toPlace with
                        Economy = Some { toEconomy with Inventory = addStock good moved toEconomy.Inventory } }

                fromPlace, toPlace, moved
        | _ -> fromPlace, toPlace, 0.0

    let private supplierIds good (places: Map<PlaceId, Place>) =
        places
        |> Map.toSeq
        |> Seq.choose (fun (placeId, place) ->
            match place.Kind, place.Economy with
            | Commercial, _ -> None
            | _, Some economy when stockOf good economy.Inventory > 1.0 -> Some placeId
            | _ -> None)
        |> Seq.toList

    let private replenishCommercial (places: Map<PlaceId, Place>) placeId (place: Place) =
        match place.Economy with
        | None -> places
        | Some economy ->
            ((places, economy.Sells) ||> List.fold (fun places offering ->
                let latestPlace = Map.find placeId places
                let latestEconomy = latestPlace.Economy |> Option.defaultValue economy
                let current = stockOf offering.Good latestEconomy.Inventory
                let deficit = max 0.0 (offering.TargetStock - current)

                if deficit <= 0.0 then
                    places
                else
                    let suppliers = supplierIds offering.Good places

                    ((places, deficit), suppliers)
                    ||> List.fold (fun (places, remaining) supplierId ->
                        if remaining <= 0.0 || supplierId = placeId then
                            places, remaining
                        else
                            let supplier = Map.find supplierId places
                            let buyer = Map.find placeId places
                            let supplier, buyer, moved = transfer offering.Good remaining supplier buyer

                            places
                            |> Map.add supplierId supplier
                            |> Map.add placeId buyer,
                            remaining - moved)
                    |> fst))

    let tickPlaces minutes cityMap =
        let producedPlaces =
            cityMap.Places
            |> Map.map (fun _ place -> updatePlace minutes place)

        let replenishedPlaces =
            producedPlaces
            |> Map.toSeq
            |> Seq.filter (fun (_, place) -> place.Kind = Commercial)
            |> Seq.fold (fun places (placeId, place) -> replenishCommercial places placeId place) producedPlaces

        { cityMap with Places = replenishedPlaces }

    let private offeringFor intent good place =
        place.Economy
        |> Option.bind (fun economy ->
            economy.Sells
            |> List.tryFind (fun offering -> offering.Good = good && offering.Intent = intent && stockOf good economy.Inventory >= 1.0))

    let findShoppingDestination cityMap sim origin good intent =
        let maxTravelMinutes =
            match intent with
            | NeedPurchase -> 25
            | WantPurchase ->
                sim.Wants
                |> List.tryFind (fun want -> want.Good = good)
                |> Option.map _.MaxTravelMinutes
                |> Option.defaultValue 90

        let candidates =
            cityMap.Places
            |> Map.toSeq
            |> Seq.choose (fun (placeId, place) ->
                offeringFor intent good place
                |> Option.bind (fun offering ->
                    MapGraph.findRoute cityMap origin placeId
                    |> Option.map (fun route -> placeId, place, offering, route)))
            |> Seq.filter (fun (_, _, _, route) -> route.TotalMinutes <= float maxTravelMinutes)
            |> Seq.toList

        match intent with
        | NeedPurchase ->
            candidates
            |> List.sortBy (fun (_, _, offering, route) -> route.TotalMinutes, offering.Price)
            |> List.tryHead
            |> Option.map (fun (placeId, _, _, route) -> placeId, int (Math.Ceiling route.TotalMinutes))
        | WantPurchase ->
            candidates
            |> List.sortByDescending (fun (_, _, offering, route) ->
                let pricePenalty = float offering.Price / 200.0 * sim.Personality.Frugality
                let travelPenalty = route.TotalMinutes / float maxTravelMinutes * (0.4 * sim.Personality.RoutinePreference)
                offering.Appeal + sim.Personality.Openness * 0.2 - pricePenalty - travelPenalty)
            |> List.tryHead
            |> Option.map (fun (placeId, _, _, route) -> placeId, int (Math.Ceiling route.TotalMinutes))

    let private updateNeed kind amount sim =
        let needs =
            sim.Needs
            |> Map.change kind (Option.map (fun need -> { need with Value = clamp01 (need.Value + amount) }))

        { sim with Needs = needs }

    let purchaseAt place sim intent good =
        match place.Economy, offeringFor intent good place with
        | Some economy, Some offering when sim.Wallet >= offering.Price ->
            let economy =
                { economy with
                    Inventory = removeStock good 1.0 economy.Inventory
                    Cash = economy.Cash + offering.Price }

            let sim =
                { sim with
                    Wallet = sim.Wallet - offering.Price
                    HouseholdInventory = addStock good 1.0 sim.HouseholdInventory
                    Happiness =
                        match intent with
                        | NeedPurchase -> clamp01 (sim.Happiness + 0.03)
                        | WantPurchase -> clamp01 (sim.Happiness + 0.10 * offering.Appeal) }

            let sim =
                match good with
                | Groceries -> updateNeed Hunger 0.45 sim
                | HouseholdGoods -> updateNeed Hygiene 0.25 sim
                | Clothing -> updateNeed Social 0.10 sim |> updateNeed Fun 0.08
                | Electronics
                | Entertainment -> updateNeed Fun 0.35 sim
                | Toys -> updateNeed Fun 0.35 sim |> updateNeed Learning 0.08
                | LuxuryGoods -> updateNeed Fun 0.20 sim |> updateNeed Social 0.10
                | RawMaterials
                | ManufacturedGoods -> sim

            { place with Economy = Some economy }, sim, true
        | _ -> place, sim, false

    let commercialSummary cityMap =
        cityMap.Places
        |> Map.toSeq
        |> Seq.choose (fun (_, place) ->
            match place.Kind, place.Economy with
            | Commercial, Some economy ->
                let goods =
                    economy.Sells
                    |> List.map (fun offering -> sprintf "%A=%.0f" offering.Good (stockOf offering.Good economy.Inventory))
                    |> String.concat ", "

                Some(sprintf "%s stocks %s" place.Name goods)
            | _ -> None)
        |> Seq.toList
