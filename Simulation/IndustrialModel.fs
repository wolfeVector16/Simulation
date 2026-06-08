namespace Simulation

open Simulation.Domain
open Simulation.Measures

module IndustrialModel =
    let private profile air ground water noise odor trucks fire explosion hazmat blight hours =
        { AirPollution = air
          GroundPollution = ground
          WaterPollution = water
          Noise = noise
          Odor = odor
          TruckTraffic = trucks
          FireRisk = fire
          ExplosionRisk = explosion
          HazardousMaterialsRisk = hazmat
          VisualBlight = blight
          OperatingHoursIntensity = hours }

    let private jobs density wage skill skillIntensity shift injury =
        { JobsPerHectare = density
          AverageWagePerDay = wage
          RequiredSkill = skill
          SkillIntensity = skillIntensity
          ShiftWorkIntensity = shift
          InjuryRisk = injury }

    let private freight inbound outbound vans rail loading curb =
        { InboundTruckTripsPerDay = inbound
          OutboundTruckTripsPerDay = outbound
          VanTripsPerDay = vans
          RailOrWaterAccessPreferred = rail
          LoadingSpaceNeed = loading
          CurbPressure = curb }

    let externalitiesFor =
        function
        | Warehouse -> profile 0.06 0.04 0.03 0.34 0.03 0.72 0.14 0.02 0.03 0.24 0.64
        | DistributionCenter -> profile 0.08 0.05 0.04 0.46 0.04 0.90 0.16 0.03 0.04 0.30 0.82
        | LastMileLogistics -> profile 0.07 0.04 0.03 0.42 0.03 0.70 0.13 0.02 0.03 0.22 0.88
        | Workshop -> profile 0.05 0.03 0.02 0.26 0.04 0.24 0.12 0.02 0.04 0.12 0.42
        | MakerSpace -> profile 0.03 0.02 0.01 0.18 0.02 0.16 0.09 0.01 0.02 0.06 0.34
        | LightManufacturing -> profile 0.12 0.08 0.05 0.38 0.08 0.42 0.20 0.04 0.08 0.18 0.58
        | FoodProduction -> profile 0.09 0.05 0.09 0.32 0.18 0.38 0.16 0.03 0.06 0.12 0.55
        | CleanManufacturing -> profile 0.04 0.03 0.03 0.24 0.02 0.34 0.12 0.02 0.04 0.08 0.52
        | ResearchAndDevelopmentFlex -> profile 0.03 0.02 0.02 0.18 0.01 0.18 0.08 0.01 0.03 0.04 0.40
        | AutoRepair -> profile 0.13 0.10 0.05 0.34 0.12 0.22 0.18 0.03 0.10 0.18 0.48
        | EquipmentYard
        | ContractorYard
        | StorageYard -> profile 0.08 0.10 0.04 0.36 0.05 0.36 0.13 0.02 0.05 0.42 0.50
        | HeavyManufacturing
        | SteelMill -> profile 0.62 0.44 0.34 0.74 0.32 0.68 0.42 0.16 0.28 0.50 0.82
        | ChemicalPlant
        | Refinery -> profile 0.70 0.62 0.48 0.70 0.58 0.62 0.52 0.48 0.76 0.46 0.86
        | Sawmill -> profile 0.28 0.18 0.12 0.62 0.16 0.52 0.36 0.06 0.10 0.34 0.64
        | GrainElevator -> profile 0.22 0.13 0.08 0.58 0.14 0.64 0.30 0.18 0.12 0.38 0.72
        | Mining
        | Quarry
        | CoalMine -> profile 0.66 0.64 0.48 0.80 0.30 0.74 0.38 0.24 0.30 0.78 0.84
        | Landfill -> profile 0.24 0.78 0.42 0.52 0.88 0.68 0.28 0.08 0.34 0.86 0.72
        | RecyclingCenter
        | WasteTransferStation -> profile 0.18 0.34 0.20 0.58 0.48 0.76 0.24 0.05 0.20 0.50 0.76
        | PowerPlant Coal -> profile 0.82 0.42 0.30 0.58 0.20 0.52 0.30 0.10 0.24 0.48 0.90
        | PowerPlant Gas -> profile 0.38 0.16 0.10 0.46 0.08 0.28 0.22 0.16 0.16 0.28 0.86
        | PowerPlant Nuclear -> profile 0.04 0.08 0.12 0.38 0.02 0.24 0.18 0.04 0.34 0.26 0.92
        | PowerPlant Solar
        | PowerPlant Wind
        | PowerPlant Hydro
        | PowerPlant BatteryStorage -> profile 0.02 0.03 0.03 0.20 0.01 0.14 0.10 0.02 0.04 0.18 0.65

    let buildingFormFor =
        function
        | Warehouse -> WarehouseBox
        | DistributionCenter
        | LastMileLogistics -> LoadingDockWarehouse
        | Workshop
        | MakerSpace
        | AutoRepair -> SmallWorkshopBuilding
        | LightManufacturing -> LightManufacturingBuilding
        | FoodProduction -> FoodProductionFacility
        | CleanManufacturing -> CleanRoomManufacturing
        | ResearchAndDevelopmentFlex -> FlexIndustrialBuilding
        | EquipmentYard
        | ContractorYard -> ContractorYardWithEquipment
        | StorageYard -> StorageYardWithSheds
        | HeavyManufacturing
        | SteelMill
        | Sawmill -> HeavyFactory
        | ChemicalPlant
        | Refinery -> TankFarm
        | GrainElevator -> GrainElevatorStructure
        | Mining
        | CoalMine -> MineHead
        | Quarry -> QuarryPit
        | Landfill -> LandfillCell
        | RecyclingCenter
        | WasteTransferStation -> WasteTransferBuilding
        | PowerPlant _ -> PowerGenerationFacility

    let jobProfileFor =
        function
        | Warehouse
        | DistributionCenter
        | LastMileLogistics -> jobs 46.0 118m None 0.30 0.72 0.18
        | Workshop
        | MakerSpace -> jobs 82.0 160m (Some Handiness) 0.64 0.28 0.10
        | LightManufacturing
        | FoodProduction -> jobs 70.0 148m (Some Handiness) 0.55 0.46 0.18
        | CleanManufacturing
        | ResearchAndDevelopmentFlex -> jobs 92.0 230m (Some Logic) 0.82 0.36 0.08
        | AutoRepair
        | EquipmentYard
        | ContractorYard
        | StorageYard -> jobs 34.0 145m (Some Handiness) 0.48 0.32 0.16
        | HeavyManufacturing
        | ChemicalPlant
        | Refinery
        | SteelMill
        | Sawmill
        | GrainElevator -> jobs 42.0 190m (Some Handiness) 0.58 0.70 0.34
        | Mining
        | Quarry
        | CoalMine -> jobs 20.0 185m (Some Fitness) 0.44 0.68 0.42
        | Landfill
        | RecyclingCenter
        | WasteTransferStation -> jobs 26.0 150m (Some Handiness) 0.38 0.62 0.24
        | PowerPlant kind ->
            match kind with
            | Solar
            | Wind
            | Hydro
            | BatteryStorage -> jobs 18.0 210m (Some Logic) 0.70 0.42 0.10
            | _ -> jobs 24.0 225m (Some Logic) 0.78 0.70 0.18

    let freightDemandFor =
        function
        | Warehouse -> freight 38.0 42.0 8.0 false 0.78 0.34
        | DistributionCenter -> freight 70.0 78.0 15.0 true 0.92 0.42
        | LastMileLogistics -> freight 18.0 24.0 85.0 false 0.62 0.88
        | Workshop
        | MakerSpace -> freight 3.0 4.0 7.0 false 0.20 0.22
        | LightManufacturing
        | FoodProduction
        | CleanManufacturing -> freight 12.0 14.0 5.0 false 0.46 0.18
        | ResearchAndDevelopmentFlex -> freight 4.0 4.0 5.0 false 0.22 0.14
        | AutoRepair
        | EquipmentYard
        | ContractorYard
        | StorageYard -> freight 7.0 8.0 10.0 false 0.40 0.34
        | HeavyManufacturing
        | ChemicalPlant
        | Refinery
        | SteelMill
        | Sawmill
        | GrainElevator -> freight 42.0 38.0 4.0 true 0.88 0.20
        | Mining
        | Quarry
        | CoalMine -> freight 55.0 62.0 2.0 true 0.94 0.10
        | Landfill
        | RecyclingCenter
        | WasteTransferStation -> freight 46.0 12.0 4.0 false 0.74 0.28
        | PowerPlant _ -> freight 12.0 5.0 2.0 true 0.58 0.08

    let compatibleZonesFor =
        function
        | Warehouse -> [ LightIndustrialZone; WarehouseLogisticsZone; IndustrialZone ] |> Set.ofList
        | DistributionCenter
        | LastMileLogistics -> [ WarehouseLogisticsZone; IndustrialZone ] |> Set.ofList
        | Workshop
        | MakerSpace -> [ MixedUseProductionZone; LightIndustrialZone; FlexIndustrialZone; CommercialZone; IndustrialZone ] |> Set.ofList
        | LightManufacturing
        | FoodProduction
        | AutoRepair -> [ LightIndustrialZone; MixedUseProductionZone; IndustrialZone ] |> Set.ofList
        | CleanManufacturing
        | ResearchAndDevelopmentFlex -> [ FlexIndustrialZone; LightIndustrialZone; OfficeZone; IndustrialZone ] |> Set.ofList
        | EquipmentYard
        | ContractorYard
        | StorageYard -> [ LightIndustrialZone; WarehouseLogisticsZone; IndustrialZone ] |> Set.ofList
        | HeavyManufacturing
        | SteelMill
        | Sawmill
        | GrainElevator -> [ HeavyIndustrialZone; IndustrialZone ] |> Set.ofList
        | ChemicalPlant
        | Refinery -> [ HazardousIndustrialZone ] |> Set.ofList
        | Mining
        | Quarry
        | CoalMine -> [ ExtractiveIndustrialZone ] |> Set.ofList
        | Landfill
        | RecyclingCenter
        | WasteTransferStation -> [ WasteManagementZone; UtilityZone ] |> Set.ofList
        | PowerPlant _ -> [ UtilityZone; IndustrialZone ] |> Set.ofList

    let residentialBufferFor useKind =
        match useKind with
        | Workshop
        | MakerSpace
        | ResearchAndDevelopmentFlex -> 20.0
        | Warehouse
        | LightManufacturing
        | CleanManufacturing
        | FoodProduction
        | AutoRepair -> 55.0
        | DistributionCenter
        | LastMileLogistics
        | EquipmentYard
        | ContractorYard
        | StorageYard -> 95.0
        | HeavyManufacturing
        | SteelMill
        | Sawmill
        | GrainElevator -> 260.0
        | ChemicalPlant
        | Refinery -> 700.0
        | Mining
        | Quarry
        | CoalMine -> 500.0
        | Landfill -> 850.0
        | RecyclingCenter
        | WasteTransferStation -> 320.0
        | PowerPlant kind ->
            match kind with
            | Solar
            | Wind
            | Hydro
            | BatteryStorage -> 80.0
            | Gas -> 280.0
            | Coal -> 700.0
            | Nuclear -> 1000.0

    let siteFor useKind =
        { Use = useKind
          Form = buildingFormFor useKind
          Externalities = externalitiesFor useKind
          Jobs = jobProfileFor useKind
          Freight = freightDemandFor useKind
          MinimumResidentialBufferMeters = residentialBufferFor useKind
          CompatibleZones = compatibleZonesFor useKind }

    let isAllowedInZone zone useKind =
        Set.contains zone (compatibleZonesFor useKind)

    let compatibility zone nearestResidentialMeters freightAccess nearby useKind =
        let site = siteFor useKind
        let zoneAllowed = isAllowedInZone zone useKind
        let externalities = site.Externalities
        let residentialConflict =
            Set.contains NearbyResidential nearby
            && nearestResidentialMeters < site.MinimumResidentialBufferMeters
        let freightConflict =
            site.Freight.LoadingSpaceNeed > 0.55
            && freightAccess < 0.50

        let warnings =
            [ if not zoneAllowed then $"%A{useKind} is not allowed in %A{zone}."
              if residentialConflict then $"Residential buffer %.0f{site.MinimumResidentialBufferMeters}m required."
              if freightConflict then "Freight access is weak for the expected truck/loading demand."
              if externalities.TruckTraffic > 0.65 then "High truck traffic expected."
              if externalities.Noise > 0.55 then "Noise mitigation or operating-hour limits are recommended."
              if externalities.HazardousMaterialsRisk > 0.40 then "Hazardous materials response planning required." ]

        { Allowed = zoneAllowed && not residentialConflict && not freightConflict
          Warnings = warnings
          RequiredBufferMeters = site.MinimumResidentialBufferMeters
          FreightAccessScore = freightAccess }

    let industrialTaxonomyCategory =
        function
        | Warehouse
        | DistributionCenter
        | LastMileLogistics -> "Warehouse/logistics"
        | Workshop
        | MakerSpace
        | AutoRepair -> "Workshop/light production"
        | LightManufacturing
        | FoodProduction -> "Light manufacturing"
        | CleanManufacturing
        | ResearchAndDevelopmentFlex -> "Clean/flex industrial"
        | EquipmentYard
        | ContractorYard
        | StorageYard -> "Industrial yard"
        | HeavyManufacturing
        | SteelMill
        | Sawmill
        | GrainElevator -> "Heavy industry"
        | ChemicalPlant
        | Refinery -> "Hazardous industry"
        | Mining
        | Quarry
        | CoalMine -> "Extractive industry"
        | Landfill
        | RecyclingCenter
        | WasteTransferStation -> "Waste management"
        | PowerPlant _ -> "Utility/power"

    let neighborhoodImpact site =
        let e = site.Externalities
        {| Pollution = clamp01 (e.AirPollution * 0.45 + e.GroundPollution * 0.35 + e.WaterPollution * 0.20)
           Traffic = clamp01 (e.TruckTraffic * 0.75 + e.OperatingHoursIntensity * 0.20)
           DesirabilityPenalty = clamp01 (e.Noise * 0.20 + e.Odor * 0.25 + e.VisualBlight * 0.25 + e.HazardousMaterialsRisk * 0.30)
           FireRisk = clamp01 (e.FireRisk * 0.70 + e.ExplosionRisk * 0.30) |}
