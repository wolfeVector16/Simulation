namespace RealSim.Tests

open Xunit
open Simulation
open Simulation.Domain
open RealSim.Avalonia.Services

module IndustrialLandUseTests =
    let private pointToSegmentDistance point a b =
        let vx = b.X - a.X
        let vy = b.Y - a.Y
        let wx = point.X - a.X
        let wy = point.Y - a.Y
        let lengthSquared = vx * vx + vy * vy
        let t =
            if lengthSquared <= 0.0001 then 0.0
            else max 0.0 (min 1.0 ((wx * vx + wy * vy) / lengthSquared))
        let projection = { X = a.X + t * vx; Y = a.Y + t * vy }
        MapGraph.distanceMeters { Places = Map.empty; RoadNodes = Map.empty; RoadSegments = []; MetersPerMapUnit = 8.0 } point projection

    let private nearFreightAccess world (parcel: Parcel) =
        world.Map.RoadSegments
        |> List.filter (fun segment ->
            segment.RoadClass = Highway
            || segment.RoadClass = IndustrialRoad
            || segment.RoadClass = FreightCorridor)
        |> List.exists (fun segment ->
            let fromNode = world.Map.RoadNodes[segment.From]
            let toNode = world.Map.RoadNodes[segment.To]
            pointToSegmentDistance parcel.Position fromNode.Position toNode.Position <= 700.0)

    [<Fact>]
    let ``WarehouseHasLowPollutionButHighTruckTraffic`` () =
        let warehouse = IndustrialModel.siteFor IndustrialUse.Warehouse
        let heavy = IndustrialModel.siteFor HeavyManufacturing

        Assert.True(warehouse.Externalities.AirPollution < 0.15)
        Assert.True(warehouse.Externalities.GroundPollution < 0.10)
        Assert.True(warehouse.Externalities.TruckTraffic > 0.65)
        Assert.True(heavy.Externalities.AirPollution > warehouse.Externalities.AirPollution * 5.0)

    [<Fact>]
    let ``WorkshopCanBeCompatibleNearCommercial`` () =
        let result =
            IndustrialModel.compatibility
                MixedUseProductionZone
                80.0
                0.60
                ([ NearbyCommercial; NearbyMixedUse ] |> Set.ofList)
                Workshop

        Assert.True(result.Allowed, String.concat "; " result.Warnings)
        Assert.True(result.RequiredBufferMeters <= 30.0)

    [<Fact>]
    let ``HeavyIndustryRequiresBuffer`` () =
        let result =
            IndustrialModel.compatibility
                HeavyIndustrialZone
                60.0
                0.80
                ([ NearbyResidential ] |> Set.ofList)
                HeavyManufacturing

        Assert.False(result.Allowed)
        Assert.Contains(result.Warnings, fun warning -> warning.Contains("Residential buffer"))
        Assert.True(result.RequiredBufferMeters >= 250.0)

    [<Fact>]
    let ``LandfillHasOdorAndGroundPollution`` () =
        let landfill = IndustrialModel.siteFor Landfill
        let warehouse = IndustrialModel.siteFor IndustrialUse.Warehouse

        Assert.True(landfill.Externalities.Odor > 0.80)
        Assert.True(landfill.Externalities.GroundPollution > 0.70)
        Assert.True(landfill.Externalities.Odor > warehouse.Externalities.Odor * 10.0)

    [<Fact>]
    let ``CleanManufacturingHasLowPollutionHighSkillJobs`` () =
        let clean = IndustrialModel.siteFor CleanManufacturing

        Assert.True(clean.Externalities.AirPollution < 0.08)
        Assert.True(clean.Jobs.SkillIntensity > 0.75)
        Assert.Equal(Some Logic, clean.Jobs.RequiredSkill)
        Assert.True(clean.Jobs.AverageWagePerDay >= 200m)

    [<Fact>]
    let ``IndustrialSubtypesCreateDifferentFreightDemand`` () =
        let warehouse = IndustrialModel.siteFor DistributionCenter
        let workshop = IndustrialModel.siteFor Workshop

        Assert.True(warehouse.Freight.InboundTruckTripsPerDay > workshop.Freight.InboundTruckTripsPerDay * 10.0)
        Assert.True(warehouse.Freight.LoadingSpaceNeed > workshop.Freight.LoadingSpaceNeed)

    [<Fact>]
    let ``ZoningAllowsLightIndustrialButRejectsHazardousUse`` () =
        Assert.True(IndustrialModel.isAllowedInZone LightIndustrialZone Workshop)
        Assert.True(IndustrialModel.isAllowedInZone LightIndustrialZone LightManufacturing)
        Assert.False(IndustrialModel.isAllowedInZone LightIndustrialZone Refinery)
        Assert.False(IndustrialModel.compatibility LightIndustrialZone 1000.0 0.90 Set.empty Refinery |> _.Allowed)

    [<Fact>]
    let ``MixedUseProductionAllowsSmallWorkshop`` () =
        let result = IndustrialModel.compatibility MixedUseProductionZone 45.0 0.50 ([ NearbyResidential ] |> Set.ofList) MakerSpace

        Assert.True(result.Allowed, String.concat "; " result.Warnings)

    [<Fact>]
    let ``WorldGenerationPlacesWarehousesNearFreightAccess`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let warehouseParcels =
            world.City.IndustrialSites
            |> Map.toSeq
            |> Seq.filter (fun (_, site) -> site.Use = IndustrialUse.Warehouse || site.Use = DistributionCenter || site.Use = LastMileLogistics)
            |> Seq.map (fun (parcelId, _) -> world.City.Parcels[parcelId])
            |> Seq.toList

        Assert.NotEmpty(warehouseParcels)
        Assert.All(warehouseParcels, fun parcel -> Assert.True(nearFreightAccess world parcel, $"{parcel.Name} should be near freight/highway/industrial road access."))

    [<Fact>]
    let ``WorldGenerationDoesNotTreatAllIndustryAsHeavyPollution`` () =
        let world = RealSim.Scenarios.Juniper.createWorld 1337
        let industrialSites = world.City.IndustrialSites |> Map.toSeq |> Seq.map snd |> Seq.toList

        Assert.Contains(industrialSites, fun site -> site.Externalities.AirPollution < 0.15 && site.Externalities.TruckTraffic > 0.60)
        Assert.Contains(industrialSites, fun site -> site.Externalities.AirPollution < 0.20 && site.Jobs.RequiredSkill.IsSome)
        Assert.True(industrialSites |> List.map (fun site -> site.Externalities.AirPollution) |> List.distinct |> List.length >= 2)

    [<Fact>]
    let ``AvaloniaProjectionDifferentiatesIndustrialSubtype`` () =
        let projection = RealSim.Scenarios.Juniper.createWorld 1337 |> MapProjection.Project
        let industrialCategories =
            projection.Primitives
            |> Seq.filter (fun primitive -> primitive.Kind = RealSim.Avalonia.Models.MapPrimitiveKind.Building)
            |> Seq.map _.Category
            |> Set.ofSeq

        Assert.Contains("Warehouse/logistics", industrialCategories)
        Assert.Contains("Light manufacturing", industrialCategories)
        Assert.DoesNotContain("Industry", industrialCategories)

    [<Fact>]
    let ``IndustrialExternalitiesAffectNeighborhoodNuanced`` () =
        let warehouse = IndustrialModel.siteFor IndustrialUse.Warehouse |> IndustrialModel.neighborhoodImpact
        let heavy = IndustrialModel.siteFor HeavyManufacturing |> IndustrialModel.neighborhoodImpact

        Assert.True(warehouse.Traffic > 0.50)
        Assert.True(warehouse.Pollution < 0.10)
        Assert.True(heavy.Pollution > warehouse.Pollution * 5.0)
        Assert.True(heavy.DesirabilityPenalty > warehouse.DesirabilityPenalty)
