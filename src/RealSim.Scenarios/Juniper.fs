namespace RealSim.Scenarios

open System
open Simulation
open Simulation.Domain
open Simulation.Measures

module Juniper =
    let private emptyEconomy =
        { Inventory = Map.empty
          Sells = []
          Produces = []
          ImportsPerDay = Map.empty
          Cash = 0m }

    let defaultNeeds =
        [ Hunger, { Value = 0.82; DecayPerHour = 0.055; CriticalBelow = 0.30 }
          Energy, { Value = 0.78; DecayPerHour = 0.045; CriticalBelow = 0.25 }
          Social, { Value = 0.62; DecayPerHour = 0.025; CriticalBelow = 0.25 }
          Hygiene, { Value = 0.76; DecayPerHour = 0.030; CriticalBelow = 0.25 }
          Fun, { Value = 0.58; DecayPerHour = 0.030; CriticalBelow = 0.25 }
          Bladder, { Value = 0.88; DecayPerHour = 0.070; CriticalBelow = 0.22 }
          Safety, { Value = 0.95; DecayPerHour = 0.005; CriticalBelow = 0.35 }
          Purpose, { Value = 0.64; DecayPerHour = 0.015; CriticalBelow = 0.28 }
          Learning, { Value = 0.50; DecayPerHour = 0.020; CriticalBelow = 0.25 }
          Comfort, { Value = 0.60; DecayPerHour = 0.018; CriticalBelow = 0.24 }
          Environment, { Value = 0.58; DecayPerHour = 0.015; CriticalBelow = 0.25 } ]
        |> Map.ofList

    let childNeeds =
        defaultNeeds
        |> Map.change Energy (Option.map (fun n -> { n with Value = 0.90; DecayPerHour = 0.060 }))
        |> Map.change Fun (Option.map (fun n -> { n with Value = 0.76; DecayPerHour = 0.050 }))
        |> Map.change Safety (Option.map (fun n -> { n with Value = 0.88; CriticalBelow = 0.45 }))
        |> Map.change Learning (Option.map (fun n -> { n with Value = 0.62; DecayPerHour = 0.030 }))

    let createSampleWorld () =
        let guidCounter = ref 0
        let nextGuid () =
            guidCounter.Value <- guidCounter.Value + 1
            Guid.Parse(sprintf "00000000-0000-0000-0000-%012x" guidCounter.Value)

        let placeId () = PlaceId(nextGuid())
        let simId () = SimId(nextGuid())
        let householdId () = HouseholdId(nextGuid())
        let householdObjectId () = HouseholdObjectId(nextGuid())
        let parcelId () = ParcelId(nextGuid())
        let relationshipId () = RelationshipId(nextGuid())
        let groupId () = GroupId(nextGuid())
        let institutionId () = InstitutionId(nextGuid())
        let settlementId () = SettlementId(nextGuid())
        let districtId () = DistrictId(nextGuid())
        let blockId () = BlockId(nextGuid())
        let jobId () = JobId(nextGuid())
        let neighborhoodId () = NeighborhoodId(nextGuid())
        let lotId () = LotId(nextGuid())
        let unitId () = UnitId(nextGuid())
        let laneId () = LaneId(nextGuid())
        let transitRouteId () = TransitRouteId(nextGuid())
        let transitStopId () = TransitStopId(nextGuid())
        let playerId () = PlayerId(nextGuid())
        let actorId () = ActorId(nextGuid())
        let itemId () = ItemId(nextGuid())
        let vehicleId () = VehicleId(nextGuid())
        let parkingZoneId () = ParkingZoneId(nextGuid())
        let roadNodeId () = RoadNodeId(nextGuid())
        let roadSegmentId () = RoadSegmentId(nextGuid())
        let signalPlanId () = SignalPlanId(nextGuid())

        let homeA = placeId ()
        let homeB = placeId ()
        let office = placeId ()
        let workshop = placeId ()
        let park = placeId ()
        let grocer = placeId ()
        let generalStore = placeId ()
        let mall = placeId ()
        let importer = placeId ()
        let elementary = placeId ()
        let daycare = placeId ()
        let policeStation = placeId ()

        let westNode = roadNodeId ()
        let midNode = roadNodeId ()
        let eastNode = roadNodeId ()
        let officeNode = roadNodeId ()
        let industryNode = roadNodeId ()
        let mallNode = roadNodeId ()
        let schoolNode = roadNodeId ()

        let roadNodes =
            [ { Id = westNode; Position = { X = 0.0; Y = 1.0 } }
              { Id = midNode; Position = { X = 6.0; Y = 1.0 } }
              { Id = eastNode; Position = { X = 12.0; Y = 1.0 } }
              { Id = officeNode; Position = { X = 18.0; Y = 6.0 } }
              { Id = industryNode; Position = { X = 12.0; Y = -4.0 } }
              { Id = mallNode; Position = { X = 104.0; Y = 5.0 } }
              { Id = schoolNode; Position = { X = 4.0; Y = 5.0 } } ]

        let roadNodeMap = roadNodes |> List.map (fun node -> node.Id, node) |> Map.ofList

        let segment name fromNode toNode speedKph capacity roadClass bikeFacility sidewalkQuality parkingRules =
            let a = roadNodeMap[fromNode]
            let b = roadNodeMap[toNode]
            let segmentId = roadSegmentId ()
            let forwardLane = laneId ()
            let reverseLane = laneId ()
            let busLane =
                if name = "Central Main Street" || name = "Civic Parkway" then
                    Some(laneId ())
                else
                    None

            let length =
                MapGraph.distanceMeters { Places = Map.empty; RoadNodes = roadNodeMap; RoadSegments = []; MetersPerMapUnit = 500.0 } a.Position b.Position

            let allowedGeneral =
                [ PrivateCar; TaxiOrRideshare; Bus; ServiceVehicle; DeliveryVehicle; EmergencyVehicle; SchoolBus ] |> Set.ofList

            let lane laneId direction laneType allowed capacityFactor =
                { Id = laneId
                  SegmentId = segmentId
                  Direction = direction
                  LaneType = laneType
                  AllowedModes = allowed
                  PermittedMovements = [ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList
                  LengthMeters = length
                  CapacityPerHour = float capacity * 60.0 * capacityFactor
                  CurrentDensity = 0.0
                  CurrentSpeedKph = speedKph
                  QueueLength = 0
                  Blocked = false }

            let lanes =
                [ lane forwardLane Forward General allowedGeneral 0.55
                  lane reverseLane Reverse General allowedGeneral 0.55 ]
                @ (busLane
                   |> Option.map (fun laneId -> lane laneId Forward BusOnly ([ Bus; EmergencyVehicle ] |> Set.ofList) 0.35)
                   |> Option.toList)

            { Id = segmentId
              Name = name
              From = fromNode
              To = toNode
              LengthMeters = length
              SpeedKph = speedKph
              IsTwoWay = true
              CapacityPerMinute = capacity
              RoadClass = roadClass
              LaneIds = [ forwardLane; reverseLane ] @ (busLane |> Option.toList)
              ParkingRules = parkingRules
              TransitLaneIds = busLane |> Option.toList
              BikeFacility = bikeFacility
              SidewalkQuality = sidewalkQuality
              Grade = 0.02
              SurfaceCondition = if roadClass = IndustrialRoad then 0.78 else 0.88
              Toll = if roadClass = Highway then Some 1.75m else None
              Restrictions = if roadClass = LocalStreet then [ NoThroughTraffic ] |> Set.ofList else Set.empty
              CurrentIncidents = Set.empty
              UnderConstruction = false
              WeatherImpact = 0.0
              NoiseOutput = if roadClass = Highway then 0.70 elif roadClass = Arterial then 0.45 else 0.25
              PollutionOutput = if roadClass = Highway then 0.65 elif roadClass = IndustrialRoad then 0.55 else 0.28 },
            lanes

        let roadSegmentBundles =
            [ segment "West Main Street" westNode midNode 35.0 90 Collector PaintedBikeLane 0.72 [ FreeOnStreet ]
              segment "Central Main Street" midNode eastNode 35.0 90 Arterial PaintedBikeLane 0.58 [ MeteredParking 1.50m; LoadingOnly ]
              segment "Civic Parkway" eastNode officeNode 45.0 120 Arterial NoBikeFacility 0.46 [ GarageAccessOnly ]
              segment "Foundry Road" eastNode industryNode 30.0 70 IndustrialRoad NoBikeFacility 0.32 [ LoadingOnly ]
              segment "School Avenue" midNode schoolNode 25.0 55 LocalStreet ProtectedBikeLane 0.84 [ FreeOnStreet; PermitOnly ]
              segment "Regional Connector" eastNode mallNode 70.0 160 Highway NoBikeFacility 0.10 [ NoParking ] ]

        let roadSegments = roadSegmentBundles |> List.map fst
        let transportLanes = roadSegmentBundles |> List.collect snd |> List.map (fun lane -> lane.Id, lane) |> Map.ofList

        let places =
            [ { Id = homeA
                Name = "Rowhouse 12"
                Kind = Residence
                Position = { X = 0.0; Y = 0.0 }
                RoadAccess = NearestRoadAccess 800.0
                Economy = None }
              { Id = homeB
                Name = "Canal Apartments"
                Kind = Residence
                Position = { X = 6.0; Y = 2.0 }
                RoadAccess = NearestRoadAccess 800.0
                Economy = None }
              { Id = office
                Name = "Civic Analytics"
                Kind = Workplace
                Position = { X = 18.0; Y = 7.0 }
                RoadAccess = NearestRoadAccess 800.0
                Economy = None }
              { Id = workshop
                Name = "Foundry Cooperative"
                Kind = Industrial
                Position = { X = 12.0; Y = -6.0 }
                RoadAccess = NearestRoadAccess 1200.0
                Economy =
                    Some
                        { emptyEconomy with
                            Inventory = [ RawMaterials, 60.0; ManufacturedGoods, 8.0 ] |> Map.ofList
                            Produces =
                                [ { Output = ManufacturedGoods
                                    UnitsPerDay = 80.0
                                    Inputs = [ RawMaterials, 0.65 ] |> Map.ofList } ]
                            Cash = 5000m } }
              { Id = park
                Name = "Juniper Park"
                Kind = Park
                Position = { X = 8.0; Y = 4.0 }
                RoadAccess = NoRoadAccess
                Economy = None }
              { Id = grocer
                Name = "Corner Market"
                Kind = Commercial
                Position = { X = 5.8; Y = 1.8 }
                RoadAccess = NearestRoadAccess 500.0
                Economy =
                    Some
                        { emptyEconomy with
                            Inventory = [ Groceries, 18.0 ] |> Map.ofList
                            Sells =
                                [ { Good = Groceries
                                    Intent = NeedPurchase
                                    Price = 18m
                                    Appeal = 0.45
                                    TargetStock = 35.0 } ]
                            Cash = 900m } }
              { Id = generalStore
                Name = "Main Street Goods"
                Kind = Commercial
                Position = { X = 11.0; Y = 1.5 }
                RoadAccess = NearestRoadAccess 700.0
                Economy =
                    Some
                        { emptyEconomy with
                            Inventory = [ HouseholdGoods, 14.0; Entertainment, 6.0; Toys, 8.0 ] |> Map.ofList
                            Sells =
                                [ { Good = HouseholdGoods
                                    Intent = NeedPurchase
                                    Price = 24m
                                    Appeal = 0.40
                                    TargetStock = 22.0 }
                                  { Good = Entertainment
                                    Intent = WantPurchase
                                    Price = 36m
                                    Appeal = 0.50
                                    TargetStock = 10.0 }
                                  { Good = Toys
                                    Intent = WantPurchase
                                    Price = 22m
                                    Appeal = 0.62
                                    TargetStock = 16.0 } ]
                            Cash = 1200m } }
              { Id = mall
                Name = "Regional Galleria"
                Kind = Commercial
                Position = { X = 104.0; Y = 6.0 }
                RoadAccess = NearestRoadAccess 1000.0
                Economy =
                    Some
                        { emptyEconomy with
                            Inventory = [ Clothing, 60.0; Electronics, 42.0; LuxuryGoods, 18.0; Entertainment, 35.0; Toys, 24.0 ] |> Map.ofList
                            Sells =
                                [ { Good = Clothing
                                    Intent = WantPurchase
                                    Price = 65m
                                    Appeal = 0.72
                                    TargetStock = 80.0 }
                                  { Good = Electronics
                                    Intent = WantPurchase
                                    Price = 220m
                                    Appeal = 0.95
                                    TargetStock = 50.0 }
                                  { Good = Entertainment
                                    Intent = WantPurchase
                                    Price = 55m
                                    Appeal = 0.88
                                    TargetStock = 60.0 }
                                  { Good = LuxuryGoods
                                    Intent = WantPurchase
                                    Price = 350m
                                    Appeal = 0.98
                                    TargetStock = 25.0 }
                                  { Good = Toys
                                    Intent = WantPurchase
                                    Price = 30m
                                    Appeal = 0.82
                                    TargetStock = 35.0 } ]
                            Cash = 15000m } }
              { Id = importer
                Name = "North Freight Terminal"
                Kind = OutsideConnection
                Position = { X = -1.5; Y = 1.0 }
                RoadAccess = NearestRoadAccess 900.0
                Economy =
                    Some
                        { emptyEconomy with
                            Inventory =
                                [ Groceries, 120.0
                                  HouseholdGoods, 80.0
                                  RawMaterials, 250.0
                                  Clothing, 90.0
                                  Electronics, 40.0
                                  Entertainment, 70.0
                                  LuxuryGoods, 20.0
                                  Toys, 45.0 ]
                                |> Map.ofList
                            ImportsPerDay =
                                [ Groceries, 160.0
                                  HouseholdGoods, 75.0
                                  RawMaterials, 220.0
                                  Clothing, 45.0
                                  Electronics, 22.0
                                  Entertainment, 35.0
                                  LuxuryGoods, 8.0
                                  Toys, 20.0 ]
                                |> Map.ofList
                            Cash = 25000m } }
              { Id = elementary
                Name = "Juniper Elementary"
                Kind = School
                Position = { X = 4.2; Y = 5.3 }
                RoadAccess = NearestRoadAccess 500.0
                Economy = None }
              { Id = daycare
                Name = "Little Steps Daycare"
                Kind = Daycare
                Position = { X = 5.0; Y = 4.4 }
                RoadAccess = NearestRoadAccess 500.0
                Economy = None }
              { Id = policeStation
                Name = "Small Police Station"
                Kind = Civic
                Position = { X = 6.5; Y = 1.0 }
                RoadAccess = NearestRoadAccess 500.0
                Economy = None } ]

        let simAId = simId ()
        let simBId = simId ()
        let childId = simId ()
        let householdA = householdId ()
        let householdB = householdId ()
        let juniperSettlement = settlementId ()
        let riverWorksSettlement = settlementId ()
        let downtownDistrict = districtId ()
        let canalDistrict = districtId ()
        let industrialDistrict = districtId ()
        let civicBlock = blockId ()
        let housingBlock = blockId ()
        let retailBlock = blockId ()
        let industrialBlock = blockId ()
        let downtown = neighborhoodId ()
        let eastMarket = neighborhoodId ()
        let iversLot = lotId ()
        let sunLot = lotId ()
        let iversUnit = unitId ()
        let sunUnit = unitId ()
        let schoolInstitution = institutionId ()
        let employerInstitution = institutionId ()
        let landlordInstitution = institutionId ()
        let transitInstitution = institutionId ()
        let policeInstitution = institutionId ()
        let familyGroup = groupId ()
        let workGroup = groupId ()
        let classGroup = groupId ()

        let householdObject name kind room quality cleanliness broken interactions =
            { Id = householdObjectId ()
              Name = name
              Kind = kind
              Room = room
              Quality = quality
              Cleanliness = cleanliness
              Broken = broken
              Interactions = interactions }

        let iversObjects =
            [ householdObject "Mara's Bed" BedObject "Bedroom" 0.72 0.88 false [ SleepInBed ]
              householdObject "Compact Fridge" FridgeObject "Kitchen" 0.58 0.82 false [ GrabSnack; CookMeal ]
              householdObject "Reliable Stove" StoveObject "Kitchen" 0.62 0.78 false [ CookMeal; CleanObject; RepairObject ]
              householdObject "Blue Tile Shower" ShowerObject "Bathroom" 0.55 0.74 false [ ShowerSelf; CleanObject; RepairObject ]
              householdObject "Small Toilet" ToiletObject "Bathroom" 0.52 0.70 false [ UseToilet; CleanObject; RepairObject ]
              householdObject "Old Sofa" SofaObject "Living Room" 0.50 0.76 false []
              householdObject "Family TV" TvObject "Living Room" 0.57 0.80 false [ WatchTv; PlayGames ]
              householdObject "Tall Bookshelf" BookshelfObject "Living Room" 0.64 0.82 false [ ReadBook; PracticeSkill Logic ]
              householdObject "Toy Box" ToyBoxObject "Lena's Room" 0.70 0.72 false [ PlayWithToys ]
              householdObject "House Plants" DecorObject "Living Room" 0.66 0.90 false [] ]

        let theoObjects =
            [ householdObject "Futon" BedObject "Studio" 0.45 0.64 false [ SleepInBed ]
              householdObject "Mini Fridge" FridgeObject "Kitchenette" 0.42 0.58 false [ GrabSnack ]
              householdObject "Basic Shower" ShowerObject "Bathroom" 0.44 0.60 false [ ShowerSelf; CleanObject; RepairObject ]
              householdObject "Basic Toilet" ToiletObject "Bathroom" 0.44 0.55 false [ UseToilet; CleanObject; RepairObject ]
              householdObject "Gaming Computer" ComputerObject "Studio" 0.80 0.62 false [ PlayGames; PracticeSkill Programming; PracticeSkill Writing ]
              householdObject "Bookshelf Crate" BookshelfObject "Studio" 0.35 0.58 false [ ReadBook ] ]

        let simA =
            { Id = simAId
              Name = "Mara Ivers"
              LifeStage = Adult
              Household = householdA
              Home = homeA
              Job =
                Some
                    { Title = "Transit Analyst"
                      Workplace = office
                      StartMinute = 9 * 60
                      EndMinute = 17 * 60
                      PayPerDay = 220m }
              School = None
              AgeDays = 14200
              Traits = [ Ambitious; Neat; FamilyOriented ]
              Skills = [ Logic, { Level = 3; Experience = 45.0 }; Cooking, { Level = 2; Experience = 20.0 } ] |> Map.ofList
              Emotion = Fine
              Moodlets = []
              Aspiration = Some { Kind = CareerSuccess; Progress = 0.32; RewardPoints = 250 }
              Fears = [ FearOfFailure; FearOfPoverty ]
              ActionQueue = []
              Memories = []
              SocialCapacity = 8
              Needs = defaultNeeds
              Personality =
                { Openness = 0.72
                  Conscientiousness = 0.81
                  Extraversion = 0.42
                  Agreeableness = 0.68
                  Neuroticism = 0.34
                  Ambition = 0.77
                  Frugality = 0.55
                  RoutinePreference = 0.74 }
              Location = AtPlace homeA
              Activity = Sleeping
              Wallet = 1400m
              Happiness = 0.75
              Guardians = []
              Dependents = [ childId ]
              Relationships = [ childId, 0.92 ] |> Map.ofList
              HouseholdInventory = Map.empty
              Wants =
                [ { Good = Electronics
                    Desire = 0.70
                    WeekendOnly = true
                    MaxTravelMinutes = 120 }
                  { Good = Clothing
                    Desire = 0.52
                    WeekendOnly = true
                    MaxTravelMinutes = 100 } ] }

        let simB =
            { Id = simBId
              Name = "Theo Sun"
              LifeStage = YoungAdult
              Household = householdB
              Home = homeB
              Job =
                Some
                    { Title = "Fabricator"
                      Workplace = workshop
                      StartMinute = 7 * 60 + 30
                      EndMinute = 15 * 60 + 30
                      PayPerDay = 180m }
              School = None
              AgeDays = 8400
              Traits = [ Outgoing; Creative; Slob ]
              Skills = [ Handiness, { Level = 2; Experience = 65.0 }; Programming, { Level = 1; Experience = 40.0 } ] |> Map.ofList
              Emotion = Fine
              Moodlets = []
              Aspiration = Some { Kind = SocialButterfly; Progress = 0.18; RewardPoints = 120 }
              Fears = [ FearOfLoneliness ]
              ActionQueue = []
              Memories = []
              SocialCapacity = 10
              Needs = defaultNeeds |> Map.change Social (Option.map (fun n -> { n with Value = 0.88 }))
              Personality =
                { Openness = 0.61
                  Conscientiousness = 0.57
                  Extraversion = 0.83
                  Agreeableness = 0.73
                  Neuroticism = 0.41
                  Ambition = 0.52
                  Frugality = 0.38
                  RoutinePreference = 0.39 }
              Location = AtPlace homeB
              Activity = Sleeping
              Wallet = 620m
              Happiness = 0.80
              Guardians = []
              Dependents = []
              Relationships = Map.empty
              HouseholdInventory = Map.empty
              Wants =
                [ { Good = Entertainment
                    Desire = 0.86
                    WeekendOnly = true
                    MaxTravelMinutes = 130 }
                  { Good = Electronics
                    Desire = 0.74
                    WeekendOnly = true
                    MaxTravelMinutes = 120 } ] }

        let child =
            { Id = childId
              Name = "Lena Ivers"
              LifeStage = Child
              Household = householdA
              Home = homeA
              Job = None
              School =
                Some
                    { School = elementary
                      Grade = "3"
                      StartMinute = 8 * 60 + 15
                      EndMinute = 15 * 60
                      NeedsEscort = false }
              AgeDays = 3300
              Traits = [ Outgoing; Creative ]
              Skills = [ Creativity, { Level = 1; Experience = 30.0 }; Logic, { Level = 1; Experience = 15.0 } ] |> Map.ofList
              Emotion = Fine
              Moodlets = []
              Aspiration = Some { Kind = KnowledgeSeeker; Progress = 0.22; RewardPoints = 80 }
              Fears = [ FearOfBadGrades ]
              ActionQueue = []
              Memories = []
              SocialCapacity = 6
              Needs = childNeeds
              Personality =
                { Openness = 0.78
                  Conscientiousness = 0.46
                  Extraversion = 0.67
                  Agreeableness = 0.70
                  Neuroticism = 0.29
                  Ambition = 0.42
                  Frugality = 0.05
                  RoutinePreference = 0.48 }
              Location = AtPlace homeA
              Activity = Sleeping
              Wallet = 18m
              Happiness = 0.82
              Guardians = [ simAId ]
              Dependents = []
              Relationships = [ simAId, 0.92 ] |> Map.ofList
              HouseholdInventory = Map.empty
              Wants =
                [ { Good = Toys
                    Desire = 0.86
                    WeekendOnly = true
                    MaxTravelMinutes = 30 } ] }

        let parcel name zone density x y building landValue desirability pollution =
            { Id = parcelId ()
              Name = name
              Zone = zone
              Density = density
              Position = { X = x; Y = y }
              Area = 1.0
              Building = building
              LandValue = landValue
              Desirability = desirability
              Pollution = pollution
              Crime = 0.08
              FireRisk = 0.10
              Powered = true
              Watered = true
              RoadConnected = true }

        let building name useKind wealth capacity occupants jobs =
            Some
                { Name = name
                  Use = useKind
                  Wealth = wealth
                  Capacity = capacity
                  Occupants = occupants
                  Jobs = jobs
                  Status = Occupied }

        let parcels =
            [ parcel "Old Rowhouse Block" ResidentialZone LowDensity 0.0 0.0 (building "Rowhouses" Housing MiddleWealth 10 3 0) 0.48 0.62 0.04
              parcel "Canal Apartments Block" ResidentialZone MediumDensity 6.0 2.0 (building "Canal Apartments" Housing MiddleWealth 32 18 0) 0.54 0.66 0.05
              parcel "North Starter Lots" ResidentialZone LowDensity 3.0 7.0 None 0.42 0.58 0.03
              parcel "Main Street Retail" CommercialZone LowDensity 6.0 1.5 (building "Local Shops" Commerce MiddleWealth 20 0 20) 0.55 0.67 0.05
              parcel "Civic Office Row" CommercialZone MediumDensity 18.0 7.0 (building "Civic Analytics Offices" Commerce MiddleWealth 60 0 60) 0.62 0.70 0.04
              parcel "Foundry District" IndustrialZone MediumDensity 12.0 -6.0 (building "Foundry Cooperative Works" Industry LowWealth 55 0 55) 0.30 0.35 0.36
              parcel "Freight Terminal Yard" IndustrialZone LowDensity -1.5 1.0 (building "North Freight Terminal" Industry LowWealth 24 0 24) 0.28 0.32 0.30
              parcel "Juniper Elementary Campus" CivicZone LowDensity 4.2 5.3 (building "Juniper Elementary" PublicService MiddleWealth 120 0 14) 0.58 0.76 0.02
              parcel "Juniper Park Commons" ParkZone LowDensity 8.0 4.0 (building "Juniper Park" Recreation MiddleWealth 0 0 0) 0.64 0.82 0.01
              parcel "South Expansion Tract" Unzoned LowDensity 10.0 -1.0 None 0.34 0.42 0.08 ]
            |> List.map (fun parcel -> parcel.Id, parcel)
            |> Map.ofList

        let parcelIdNamed name =
            parcels
            |> Map.toSeq
            |> Seq.find (fun (_, parcel) -> parcel.Name = name)
            |> fst

        let utilities =
            [ { Name = "Gas Peaker Plant"
                Kind = PowerUtility
                Capacity = 220.0
                Used = 0.0
                MonthlyCost = 950m
                Pollution = 0.24
                Position = { X = 13.5; Y = -7.0 } }
              { Name = "Water Tower"
                Kind = WaterUtility
                Capacity = 95.0
                Used = 0.0
                MonthlyCost = 420m
                Pollution = 0.01
                Position = { X = 2.5; Y = 4.0 } }
              { Name = "Sewage Lagoon"
                Kind = SewageUtility
                Capacity = 80.0
                Used = 0.0
                MonthlyCost = 360m
                Pollution = 0.18
                Position = { X = 14.0; Y = -8.0 } }
              { Name = "County Landfill"
                Kind = GarbageUtility
                Capacity = 70.0
                Used = 0.0
                MonthlyCost = 280m
                Pollution = 0.20
                Position = { X = -3.0; Y = -4.0 } } ]

        let services =
            [ { Name = "Small Police Station"
                Kind = PoliceService
                CoverageRadius = 9.0
                Capacity = 80.0
                Used = 0.0
                MonthlyCost = 520m
                Effectiveness = 0.72
                Position = { X = 6.5; Y = 1.0 } }
              { Name = "Volunteer Fire House"
                Kind = FireService
                CoverageRadius = 8.0
                Capacity = 60.0
                Used = 0.0
                MonthlyCost = 480m
                Effectiveness = 0.68
                Position = { X = 5.5; Y = 1.2 } }
              { Name = "Neighborhood Clinic"
                Kind = HealthService
                CoverageRadius = 7.0
                Capacity = 90.0
                Used = 0.0
                MonthlyCost = 650m
                Effectiveness = 0.70
                Position = { X = 4.0; Y = 2.0 } }
              { Name = "Juniper Elementary"
                Kind = EducationService
                CoverageRadius = 10.0
                Capacity = 140.0
                Used = 0.0
                MonthlyCost = 820m
                Effectiveness = 0.76
                Position = { X = 4.2; Y = 5.3 } }
              { Name = "Juniper Park"
                Kind = ParkService
                CoverageRadius = 8.0
                Capacity = 300.0
                Used = 0.0
                MonthlyCost = 180m
                Effectiveness = 0.82
                Position = { X = 8.0; Y = 4.0 } }
              { Name = "Two Bus Routes"
                Kind = TransitService
                CoverageRadius = 11.0
                Capacity = 280.0
                Used = 0.0
                MonthlyCost = 390m
                Effectiveness = 0.55
                Position = { X = 6.0; Y = 1.0 } } ]

        let initialIndicators =
            { Population = 0
              Jobs = 0
              Unemployment = 0.0
              AverageLandValue = 0.0
              AverageDesirability = 0.0
              Pollution = 0.0
              Crime = 0.0
              FireRisk = 0.0
              Education = 0.0
              Health = 0.0
              Traffic = 0.0 }

        let city =
            { Name = "Juniper Falls"
              Parcels = parcels
              Utilities = utilities
              Services = services
              Budget =
                { Treasury = 12500m
                  MonthlyIncome = 0m
                  MonthlyExpenses = 0m
                  Taxes = { Residential = 0.09; Commercial = 0.095; Industrial = 0.10 }
                  Debt = 0m
                  InterestRate = 0.045 }
              Demand = { Residential = 0.42; Commercial = 0.28; Industrial = 0.30 }
              Policies =
                { RecyclingProgram = true
                  SmokeDetectors = true
                  CarpoolIncentives = false
                  CleanAirAct = false
                  EducationCampaign = true }
              Indicators = initialIndicators
              Advisors = [] }
            |> CitySystems.tick 0

        let households =
            [ householdA,
              { Id = householdA
                Name = "Ivers Household"
                Home = homeA
                Members = [ simAId; childId ] |> Set.ofList
                Funds = 3200m
                MonthlyIncome = 4200m
                MonthlyExpenses = 2550m
                RentMonthly = Some 1450m
                Debt = 6000m
                Assets = 9200m
                Benefits = 0m
                HousingStatus = Rents
                CareObligations = [ childId ] |> Set.ofList
                ChoresBacklog = 0.25
                FoodSecurity = 0.78
                TransportationAccess = 0.64
                Stability = 0.72
                ConflictLevel = 0.12
                SharedMemories = []
                SharedGoals = [ "keep stable housing"; "support Lena's school" ]
                Objects = iversObjects
                BillsDue = 0m
                Cleanliness = 0.82
                LotValue = 92500m }
              householdB,
              { Id = householdB
                Name = "Sun Household"
                Home = homeB
                Funds = 880m
                Members = [ simBId ] |> Set.ofList
                MonthlyIncome = 3100m
                MonthlyExpenses = 1900m
                RentMonthly = Some 1125m
                Debt = 1800m
                Assets = 2100m
                Benefits = 0m
                HousingStatus = Rents
                CareObligations = Set.empty
                ChoresBacklog = 0.52
                FoodSecurity = 0.58
                TransportationAccess = 0.71
                Stability = 0.54
                ConflictLevel = 0.18
                SharedMemories = []
                SharedGoals = [ "build savings"; "stay close to work" ]
                Objects = theoObjects
                BillsDue = 0m
                Cleanliness = 0.58
                LotValue = 41000m } ]
            |> Map.ofList

        let relationDimensions affection trust obligation familiarity conflict =
            { Affection = affection
              Trust = trust
              Attraction = 0.0
              Respect = trust
              Fear = 0.0
              Obligation = obligation
              Dependence = obligation * 0.5
              Resentment = conflict
              Familiarity = familiarity
              PowerImbalance = 0.0
              Loyalty = affection * 0.8
              Reputation = trust
              Conflict = conflict }

        let relationships =
            [ let maraToLena = relationshipId ()
              maraToLena,
              { Id = maraToLena
                From = simAId
                Toward = childId
                Kinds = [ ParentOf; CaregiverOf ] |> Set.ofList
                Strength = CloseTie
                Dimensions = relationDimensions 0.92 0.86 0.95 0.95 0.04
                LastInteractionDay = Some 5 }

              let lenaToMara = relationshipId ()
              lenaToMara,
              { Id = lenaToMara
                From = childId
                Toward = simAId
                Kinds = [ ChildOf; DependentOf ] |> Set.ofList
                Strength = CloseTie
                Dimensions = relationDimensions 0.90 0.88 0.70 0.94 0.03
                LastInteractionDay = Some 5 }

              let maraTheo = relationshipId ()
              maraTheo,
              { Id = maraTheo
                From = simAId
                Toward = simBId
                Kinds = [ NeighborOf; CommunityMemberWith ] |> Set.ofList
                Strength = WeakTie
                Dimensions = relationDimensions 0.20 0.35 0.05 0.30 0.02
                LastInteractionDay = None } ]
            |> Map.ofList

        let groups =
            [ familyGroup,
              { Id = familyGroup
                Name = "Ivers care network"
                Kind = CareNetwork
                Members = [ simAId; childId ] |> Set.ofList
                SharedNorms = [ HelpFamily; ShareChildcare ] |> Set.ofList
                Cohesion = 0.82
                InternalConflict = 0.10
                StatusHierarchy = [ simAId, 0.75; childId, 0.25 ] |> Map.ofList
                MeetingFrequencyDays = 1
                TrustLevel = 0.84
                SharedMemories = []
                LocalReputation = 0.65 }
              workGroup,
              { Id = workGroup
                Name = "Foundry day shift"
                Kind = WorkTeam
                Members = [ simBId ] |> Set.ofList
                SharedNorms = [ PayDebts; KeepUpAppearances ] |> Set.ofList
                Cohesion = 0.48
                InternalConflict = 0.22
                StatusHierarchy = [ simBId, 0.40 ] |> Map.ofList
                MeetingFrequencyDays = 1
                TrustLevel = 0.52
                SharedMemories = []
                LocalReputation = 0.55 }
              classGroup,
              { Id = classGroup
                Name = "Juniper Elementary grade 3"
                Kind = SchoolClass
                Members = [ childId ] |> Set.ofList
                SharedNorms = [ AttendMeetings; KeepUpAppearances ] |> Set.ofList
                Cohesion = 0.56
                InternalConflict = 0.18
                StatusHierarchy = [ childId, 0.45 ] |> Map.ofList
                MeetingFrequencyDays = 1
                TrustLevel = 0.60
                SharedMemories = []
                LocalReputation = 0.62 } ]
            |> Map.ofList

        let institutions =
            [ schoolInstitution,
              { Id = schoolInstitution
                Name = "Juniper Elementary"
                Kind = SchoolInstitution
                Place = Some elementary
                Neighborhood = downtown
                Capacity = 140
                Funding = 820m
                Quality = 0.66
                StaffLevel = 0.74
                Trust = 0.62
                EligibilityRules = [ ChildrenOnly; ResidentsOnly downtown ]
                Backlog = 8
                ServiceTimeMinutes = 390
                Cost = 0m
                Reputation = 0.68
                FailureModes = [ OvercrowdedClassrooms; LimitedCounselorTime ] }
              employerInstitution,
              { Id = employerInstitution
                Name = "Foundry Cooperative"
                Kind = EmployerInstitution
                Place = Some workshop
                Neighborhood = eastMarket
                Capacity = 55
                Funding = 5000m
                Quality = 0.54
                StaffLevel = 0.70
                Trust = 0.48
                EligibilityRules = [ EmployeesOnly ]
                Backlog = 2
                ServiceTimeMinutes = 480
                Cost = 0m
                Reputation = 0.50
                FailureModes = [ ShiftInstability; InjuryRisk ] }
              landlordInstitution,
              { Id = landlordInstitution
                Name = "Canal Property Holdings"
                Kind = LandlordInstitution
                Place = None
                Neighborhood = downtown
                Capacity = 20
                Funding = 12000m
                Quality = 0.42
                StaffLevel = 0.55
                Trust = 0.38
                EligibilityRules = [ RequiresFee 50m ]
                Backlog = 6
                ServiceTimeMinutes = 10080
                Cost = 50m
                Reputation = 0.36
                FailureModes = [ DelayedRepairs; RentHikes; EvictionFilings ] }
              transitInstitution,
              { Id = transitInstitution
                Name = "Juniper Transit Authority"
                Kind = TransitInstitution
                Place = None
                Neighborhood = downtown
                Capacity = 260
                Funding = 390m
                Quality = 0.57
                StaffLevel = 0.62
                Trust = 0.51
                EligibilityRules = [ OpenAccess ]
                Backlog = 4
                ServiceTimeMinutes = 15
                Cost = 2.25m
                Reputation = 0.52
                FailureModes = [ BusBunching; MissedConnections; LimitedEveningService ] } ]
              @ [ policeInstitution,
                  { Id = policeInstitution
                    Name = "Small Police Station"
                    Kind = PoliceInstitution
                    Place = Some policeStation
                    Neighborhood = downtown
                    Capacity = 80
                    Funding = 520m
                    Quality = 0.72
                    StaffLevel = 0.70
                    Trust = 0.56
                    EligibilityRules = [ OpenAccess ]
                    Backlog = 1
                    ServiceTimeMinutes = 15
                    Cost = 0m
                    Reputation = 0.58
                    FailureModes = [ Understaffing; ServiceBacklog ] } ]
            |> Map.ofList

        let housingUnits =
            [ iversUnit,
              { Id = iversUnit
                Lot = iversLot
                Neighborhood = downtown
                Owner = InstitutionOwner landlordInstitution
                Occupants = [ householdA ] |> Set.ofList
                RentMonthly = Some 1450m
                MortgageMonthly = None
                Condition = 0.72
                SoftCapacity = 3
                HardCapacity = 5
                UtilityAccess = [ PowerUtility; WaterUtility; SewageUtility; GarbageUtility ] |> Set.ofList
                LegalStatus = LeaseActive
                Habitability = 0.78
                EvictionRisk = 0.08
                Vacancy = false }
              sunUnit,
              { Id = sunUnit
                Lot = sunLot
                Neighborhood = downtown
                Owner = InstitutionOwner landlordInstitution
                Occupants = [ householdB ] |> Set.ofList
                RentMonthly = Some 1125m
                MortgageMonthly = None
                Condition = 0.58
                SoftCapacity = 1
                HardCapacity = 3
                UtilityAccess = [ PowerUtility; WaterUtility; SewageUtility; GarbageUtility ] |> Set.ofList
                LegalStatus = LeaseActive
                Habitability = 0.64
                EvictionRisk = 0.18
                Vacancy = false } ]
            |> Map.ofList

        let neighborhoods =
            [ downtown,
              { Id = downtown
                Name = "Downtown Juniper"
                Residents = [ householdA; householdB ] |> Set.ofList
                Lots = [ iversLot; sunLot ] |> Set.ofList
                Institutions = [ schoolInstitution; landlordInstitution; transitInstitution; policeInstitution ] |> Set.ofList
                Businesses = [ grocer; generalStore; office ] |> Set.ofList
                LandValue = 0.55
                RentPressure = 0.62
                Safety = 0.68
                Pollution = 0.18
                Walkability = 0.70
                TransitAccess = 0.56
                SocialCohesion = 0.58
                Reputation = 0.60
                VacancyRate = 0.05
                SchoolAccess = 0.72
                HealthAccess = 0.64
                EmploymentAccess = 0.58
                ServiceQuality = 0.61
                InstitutionalTrust = 0.50
                InformalSupportCapacity = 0.48
                SharedMemories = [] }
              eastMarket,
              { Id = eastMarket
                Name = "East Market Industrial"
                Residents = Set.empty
                Lots = Set.empty
                Institutions = [ employerInstitution; transitInstitution ] |> Set.ofList
                Businesses = [ workshop; importer ] |> Set.ofList
                LandValue = 0.34
                RentPressure = 0.30
                Safety = 0.52
                Pollution = 0.48
                Walkability = 0.34
                TransitAccess = 0.38
                SocialCohesion = 0.36
                Reputation = 0.42
                VacancyRate = 0.12
                SchoolAccess = 0.18
                HealthAccess = 0.25
                EmploymentAccess = 0.72
                ServiceQuality = 0.41
                InstitutionalTrust = 0.40
                InformalSupportCapacity = 0.22
                SharedMemories = [] } ]
            |> Map.ofList

        let geography =
            { Terrain = RiverValley
              Features =
                [ { Name = "Juniper River floodplain"
                    Kind = Floodplain
                    Center = { X = 5.0; Y = 0.6 }
                    RadiusMeters = 1800.0
                    BarrierStrength = 0.35
                    AmenityValue = 0.30
                    FloodRisk = 0.42
                    PollutionBuffer = 0.10 }
                  { Name = "Old canal corridor"
                    Kind = River
                    Center = { X = 6.0; Y = 1.0 }
                    RadiusMeters = 900.0
                    BarrierStrength = 0.28
                    AmenityValue = 0.22
                    FloodRisk = 0.30
                    PollutionBuffer = 0.06 }
                  { Name = "Juniper Park commons"
                    Kind = Parkland
                    Center = { X = 8.0; Y = 4.0 }
                    RadiusMeters = 650.0
                    BarrierStrength = 0.05
                    AmenityValue = 0.78
                    FloodRisk = 0.06
                    PollutionBuffer = 0.28 } ]
              BuildableLandRatio = 0.68
              WaterAccess = 0.54
              FloodRisk = 0.24
              NaturalBarrierStrength = 0.31
              OpenSpaceRatio = 0.18 }

        let settlements =
            [ juniperSettlement,
              { Id = juniperSettlement
                Name = "Juniper Falls"
                SettlementType = SmallTown
                Archetype = HistoricGrid
                Center = { X = 6.0; Y = 1.8 }
                PopulationTarget = 3800
                EmploymentTarget = 1250
                RoadPattern = "legacy main-street grid with canal breaks"
                BlockSizeMeters = 135.0
                DefaultDensity = "low-to-medium"
                LandUseMix =
                    [ MultifamilyResidential, 0.22
                      SingleFamilyResidential, 0.24
                      MixedUse, 0.12
                      NeighborhoodCommercial, 0.12
                      CivicAdministrative, 0.10
                      ParkOpenSpace, 0.08
                      IndustrialUse, 0.12 ]
                    |> Map.ofList
                MedianIncome = 54000m
                TransitViability = 0.56
                Walkability = 0.68
                ParkingDependence = 0.48
                HistoricalGrowthPhase = "rail/canal town with downtown grid and postwar apartment corridor" }
              riverWorksSettlement,
              { Id = riverWorksSettlement
                Name = "River Works Edge"
                SettlementType = IndustrialDistrict
                Archetype = IndustrialWaterfront
                Center = { X = 12.0; Y = -5.0 }
                PopulationTarget = 120
                EmploymentTarget = 420
                RoadPattern = "freight-oriented arterial spurs"
                BlockSizeMeters = 420.0
                DefaultDensity = "industrial large-parcel"
                LandUseMix =
                    [ IndustrialUse, 0.45
                      WarehouseLogistics, 0.35
                      UtilityUse, 0.10
                      ParkingUse, 0.05
                      LandUse.Vacant, 0.05 ]
                    |> Map.ofList
                MedianIncome = 42000m
                TransitViability = 0.30
                Walkability = 0.22
                ParkingDependence = 0.78
                HistoricalGrowthPhase = "legacy foundry and freight yard separated from housing by rail-era infrastructure" } ]
            |> Map.ofList

        let districts =
            [ downtownDistrict,
              { Id = downtownDistrict
                Settlement = juniperSettlement
                Name = "Downtown Juniper Grid"
                Archetype = DowntownCore
                Center = { X = 6.0; Y = 2.0 }
                Neighborhoods = [ downtown ] |> Set.ofList
                DominantLandUses = [ MixedUse; NeighborhoodCommercial; CivicAdministrative; MultifamilyResidential ] |> Set.ofList
                RoadClasses = [ LocalStreet; Collector; Arterial; TransitCorridor ] |> Set.ofList
                TransitPriority = 0.62
                FreightPriority = 0.22
                ParkingSupplyBias = 0.42
                BuildingAgeRange = (45, 115)
                IncomeBandLabel = "mixed middle/lower-middle"
                GrowthPressure = 0.48
                HistoricConstraint = Some "short blocks and canal frontage limit widening" }
              canalDistrict,
              { Id = canalDistrict
                Settlement = juniperSettlement
                Name = "Canal Apartment Corridor"
                Archetype = AgingApartmentCorridor
                Center = { X = 4.0; Y = 2.8 }
                Neighborhoods = [ downtown ] |> Set.ofList
                DominantLandUses = [ MultifamilyResidential; SchoolUse; ParkOpenSpace ] |> Set.ofList
                RoadClasses = [ Collector; LocalStreet; BikePath ] |> Set.ofList
                TransitPriority = 0.54
                FreightPriority = 0.10
                ParkingSupplyBias = 0.36
                BuildingAgeRange = (35, 75)
                IncomeBandLabel = "rent-burdened mixed income"
                GrowthPressure = 0.58
                HistoricConstraint = Some "older apartments near canal and school catchment" }
              industrialDistrict,
              { Id = industrialDistrict
                Settlement = riverWorksSettlement
                Name = "Foundry and Freight Edge"
                Archetype = WarehouseDistrict
                Center = { X = 12.0; Y = -5.4 }
                Neighborhoods = [ eastMarket ] |> Set.ofList
                DominantLandUses = [ IndustrialUse; WarehouseLogistics; UtilityUse ] |> Set.ofList
                RoadClasses = [ IndustrialRoad; FreightCorridor; Highway ] |> Set.ofList
                TransitPriority = 0.26
                FreightPriority = 0.82
                ParkingSupplyBias = 0.70
                BuildingAgeRange = (25, 95)
                IncomeBandLabel = "low-wage employment district"
                GrowthPressure = 0.24
                HistoricConstraint = Some "freight access preserved at cost of pedestrian comfort" } ]
            |> Map.ofList

        let parcelsNamed names =
            parcels
            |> Map.toSeq
            |> Seq.choose (fun (parcelId, parcel) -> if Set.contains parcel.Name names then Some parcelId else None)
            |> Set.ofSeq

        let roadFrontage names =
            roadSegments
            |> List.choose (fun segment -> if Set.contains segment.Name names then Some segment.Id else None)
            |> Set.ofList

        let blocks =
            [ housingBlock,
              { Id = housingBlock
                District = canalDistrict
                Name = "Canal housing catchment"
                Parcels = parcelsNamed ([ "Old Rowhouse Block"; "Canal Apartments Block"; "North Starter Lots" ] |> Set.ofList)
                BoundaryCenter = { X = 3.0; Y = 2.8 }
                ApproxAreaSqMeters = 56000.0
                DominantUse = MultifamilyResidential
                RoadFrontage = roadFrontage ([ "West Main Street"; "School Avenue" ] |> Set.ofList)
                PedestrianConnectivity = 0.70
                ParkingSupply = 34
                Buildable = true }
              retailBlock,
              { Id = retailBlock
                District = downtownDistrict
                Name = "Main Street retail block"
                Parcels = parcelsNamed ([ "Main Street Retail"; "Civic Office Row"; "Juniper Park Commons"; "Juniper Elementary Campus" ] |> Set.ofList)
                BoundaryCenter = { X = 8.0; Y = 3.4 }
                ApproxAreaSqMeters = 74000.0
                DominantUse = MixedUse
                RoadFrontage = roadFrontage ([ "Central Main Street"; "Civic Parkway"; "School Avenue" ] |> Set.ofList)
                PedestrianConnectivity = 0.76
                ParkingSupply = 28
                Buildable = true }
              industrialBlock,
              { Id = industrialBlock
                District = industrialDistrict
                Name = "Foundry freight superblock"
                Parcels = parcelsNamed ([ "Foundry District"; "Freight Terminal Yard"; "South Expansion Tract" ] |> Set.ofList)
                BoundaryCenter = { X = 9.5; Y = -3.8 }
                ApproxAreaSqMeters = 142000.0
                DominantUse = WarehouseLogistics
                RoadFrontage = roadFrontage ([ "Foundry Road"; "Regional Connector"; "Central Main Street" ] |> Set.ofList)
                PedestrianConnectivity = 0.28
                ParkingSupply = 62
                Buildable = true }
              civicBlock,
              { Id = civicBlock
                District = downtownDistrict
                Name = "Civic service block"
                Parcels = parcelsNamed ([ "Juniper Elementary Campus"; "Juniper Park Commons" ] |> Set.ofList)
                BoundaryCenter = { X = 5.8; Y = 5.0 }
                ApproxAreaSqMeters = 42000.0
                DominantUse = CivicAdministrative
                RoadFrontage = roadFrontage ([ "School Avenue"; "Central Main Street" ] |> Set.ofList)
                PedestrianConnectivity = 0.82
                ParkingSupply = 16
                Buildable = true } ]
            |> Map.ofList

        let generatedJobs =
            [ let foundryJob = jobId ()
              foundryJob,
              { Id = foundryJob
                Employer = Some employerInstitution
                Place = workshop
                Kind = "industrial fabrication"
                WagePerDay = 180m
                RequiredSkill = Some Handiness
                StartMinute = 7 * 60 + 30
                EndMinute = 15 * 60 + 30
                Stability = 0.62
                CommuteSensitivity = 0.74 }
              let officeJob = jobId ()
              officeJob,
              { Id = officeJob
                Employer = Some employerInstitution
                Place = office
                Kind = "civic analytics office"
                WagePerDay = 220m
                RequiredSkill = Some Logic
                StartMinute = 9 * 60
                EndMinute = 17 * 60
                Stability = 0.78
                CommuteSensitivity = 0.68 }
              let schoolJob = jobId ()
              schoolJob,
              { Id = schoolJob
                Employer = Some schoolInstitution
                Place = elementary
                Kind = "education/public service"
                WagePerDay = 155m
                RequiredSkill = Some Charisma
                StartMinute = 7 * 60 + 45
                EndMinute = 15 * 60 + 45
                Stability = 0.82
                CommuteSensitivity = 0.58 } ]
            |> Map.ofList

        let stopHomeA = transitStopId ()
        let stopHomeB = transitStopId ()
        let stopSchool = transitStopId ()
        let stopOffice = transitStopId ()
        let stopFoundry = transitStopId ()
        let trunkBus = transitRouteId ()
        let officeParking = parkingZoneId ()
        let foundryParking = parkingZoneId ()
        let schoolParking = parkingZoneId ()

        let transitStops =
            [ stopHomeA,
              { Id = stopHomeA
                Name = "Rowhouse stop"
                Place = Some homeA
                Node = Some westNode
                Position = { X = 0.2; Y = 0.9 }
                Accessibility = 0.68
                PerceivedSafety = 0.62 }
              stopHomeB,
              { Id = stopHomeB
                Name = "Canal Apartments stop"
                Place = Some homeB
                Node = Some midNode
                Position = { X = 6.0; Y = 1.4 }
                Accessibility = 0.76
                PerceivedSafety = 0.66 }
              stopSchool,
              { Id = stopSchool
                Name = "Elementary stop"
                Place = Some elementary
                Node = Some schoolNode
                Position = { X = 4.1; Y = 5.0 }
                Accessibility = 0.82
                PerceivedSafety = 0.74 }
              stopOffice,
              { Id = stopOffice
                Name = "Civic campus stop"
                Place = Some office
                Node = Some officeNode
                Position = { X = 18.0; Y = 6.3 }
                Accessibility = 0.62
                PerceivedSafety = 0.58 }
              stopFoundry,
              { Id = stopFoundry
                Name = "Foundry gate stop"
                Place = Some workshop
                Node = Some industryNode
                Position = { X = 12.0; Y = -4.4 }
                Accessibility = 0.48
                PerceivedSafety = 0.46 } ]
            |> Map.ofList

        let transitRoutes =
            [ trunkBus,
              { Id = trunkBus
                Name = "Route 2 Main-Civic-Foundry"
                Mode = Bus
                Stops = [ stopHomeA; stopHomeB; stopSchool; stopOffice; stopFoundry ]
                HeadwayMinutes = 24
                ServiceStartMinute = 5 * 60 + 30
                ServiceEndMinute = 22 * 60
                Fare = 2.25m
                Capacity = 42
                Reliability = 0.62
                DedicatedRightOfWay = false
                SignalPriority = true
                Crowding = 0.58 } ]
            |> Map.ofList

        let region =
            { Name = "Juniper Valley micro-region"
              Scenario = OldIndustrialRiverCity
              Geography = geography
              Settlements = [ juniperSettlement; riverWorksSettlement ] |> Set.ofList
              RegionalCorridors =
                roadSegments
                |> List.choose (fun segment ->
                    if segment.RoadClass = Highway || segment.RoadClass = FreightCorridor || segment.Name = "Regional Connector" then
                        Some segment.Id
                    else
                        None)
                |> Set.ofList
              TransitCorridors = [ trunkBus ] |> Set.ofList
              EconomicRole = "small county service town with legacy foundry, freight terminal, and regional retail draw"
              HistoricalNarrative = "Juniper Falls grew at a canal crossing, added an industrial edge during the rail era, and now struggles with aging apartments, constrained downtown parking, and unreliable bus access." }

        let parkingZones =
            [ officeParking,
              { Id = officeParking
                Name = "Civic garage"
                NearPlace = Some office
                Capacity = 8
                Occupied = 7
                PricePerHour = 5.50m
                AverageSearchMinutes = 9
                PermitRequired = false
                IllegalParkingRisk = 0.18
                WalkingDistanceMeters = 280.0 }
              foundryParking,
              { Id = foundryParking
                Name = "Foundry gravel lot"
                NearPlace = Some workshop
                Capacity = 18
                Occupied = 10
                PricePerHour = 0.0m
                AverageSearchMinutes = 3
                PermitRequired = false
                IllegalParkingRisk = 0.05
                WalkingDistanceMeters = 120.0 }
              schoolParking,
              { Id = schoolParking
                Name = "School curb zone"
                NearPlace = Some elementary
                Capacity = 5
                Occupied = 4
                PricePerHour = 0.0m
                AverageSearchMinutes = 7
                PermitRequired = false
                IllegalParkingRisk = 0.22
                WalkingDistanceMeters = 90.0 } ]
            |> Map.ofList

        let lanesTouching nodeId =
            roadSegments
            |> List.filter (fun segment -> segment.From = nodeId || segment.To = nodeId)
            |> List.collect _.LaneIds
            |> Set.ofList

        let signalPhases =
            [ { Kind = ThroughPhase
                DurationSeconds = 38
                Movements = [ MoveThrough; MoveRight ] |> Set.ofList }
              { Kind = ProtectedLeftPhase
                DurationSeconds = 14
                Movements = [ MoveLeft ] |> Set.ofList }
              { Kind = PedestrianCrossingPhase
                DurationSeconds = 18
                Movements = [ MoveThrough ] |> Set.ofList }
              { Kind = TransitPriorityPhase
                DurationSeconds = 8
                Movements = [ MoveThrough; MoveRight ] |> Set.ofList } ]

        let intersection nodeId control capacity mergeDifficulty safety =
            let touching = lanesTouching nodeId

            { Node = nodeId
              IncomingLanes = touching
              OutgoingLanes = touching
              PermittedMovements = touching |> Seq.map (fun lane -> lane, ([ MoveThrough; MoveLeft; MoveRight; MergeLeft; MergeRight ] |> Set.ofList)) |> Map.ofSeq
              Control = control
              SignalPhases = signalPhases
              CrosswalkQuality = safety
              BikeCrossingQuality = safety * 0.78
              CapacityPerMinute = capacity
              QueueSpillbackRisk = clamp01 (1.0 - safety + mergeDifficulty * 0.35)
              MergeDifficulty = mergeDifficulty
              VisibilitySafety = safety
              IncidentRisk = clamp01 (0.08 + mergeDifficulty * 0.18 + (1.0 - safety) * 0.12) }

        let signalPlan = signalPlanId ()

        let intersections =
            [ westNode, intersection westNode StopSign 35 0.20 0.70
              midNode, intersection midNode (Signalized signalPlan) 62 0.48 0.58
              eastNode, intersection eastNode (Signalized signalPlan) 58 0.64 0.50
              officeNode, intersection officeNode (Signalized signalPlan) 44 0.58 0.52
              industryNode, intersection industryNode Yield 32 0.50 0.42
              schoolNode, intersection schoolNode PedestrianCrossing 28 0.24 0.82
              mallNode, intersection mallNode RampMeter 70 0.62 0.56 ]
            |> Map.ofList

        let access downtownJob downtownSchool downtownFood transit walk bike parking =
            { JobAccess = downtownJob
              SchoolAccess = downtownSchool
              HealthcareAccess = 0.56
              FoodAccess = downtownFood
              SocialAccess = 0.55
              EmergencyAccess = 0.62
              FreightAccess = 0.50
              ParkingAccess = parking
              TransitReliability = transit
              WalkSafety = walk
              BikeSafety = bike
              OpportunityAccess = [ downtownJob; downtownSchool; downtownFood; transit; walk ] |> List.average }

        let transportMetrics =
            { AverageCongestion = 0.0
              AverageTravelReliability = 0.65
              AverageParkingPressure = 0.0
              TransitTrust = 0.62
              FreightReliability = 0.72
              EmergencyResponseRisk = 0.18
              LateArrivalsToday = 0
              FailedLaneChangesToday = 0
              MissedTransfersToday = 0
              ParkingFailuresToday = 0 }

        let transport =
            { Lanes = transportLanes
              Intersections = intersections
              TransitStops = transitStops
              TransitRoutes = transitRoutes
              ParkingZones = parkingZones
              Trips = Map.empty
              Movements = Map.empty
              Vehicles = Map.empty
              Incidents = Map.empty
              AccessByNeighborhood =
                [ downtown, access 0.62 0.74 0.76 0.62 0.66 0.48 0.42
                  eastMarket, access 0.72 0.18 0.30 0.48 0.34 0.22 0.61 ]
                |> Map.ofList
              SegmentCongestion = roadSegments |> List.map (fun segment -> segment.Id, 0.0) |> Map.ofList
              TravelTimeReliability = Map.empty
              RecentEvents = []
              Metrics = transportMetrics }

        let player = playerId ()
        let maraActor = actorId ()
        let theoActor = actorId ()
        let scenarioActor = actorId ()
        let parkedVehicle = vehicleId ()
        let starterGood =
            { Id = itemId ()
              Name = "bag of groceries"
              Category = PurchasedGood
              Good = Some Groceries
              Price = 18m
              OwnerLabel = Some "Corner Market" }

        let streetActors =
            [ maraActor,
              { Id = maraActor
                PersonId = Some simAId
                HouseholdId = Some householdA
                Name = "Mara Ivers"
                Location = ActorAtPlace homeA
                CurrentActivity = ActorIdle
                Control = PlayerControlled player
                Health = Healthy
                LegalStatus = NoLegalConcern
                Heat = NoHeat
                Reputation = 0.55
                Relationships = Map.empty
                Memories = []
                Inventory = Map.empty
                CurrentVehicle = None
                ActiveTrip = None }
              theoActor,
              { Id = theoActor
                PersonId = Some simBId
                HouseholdId = Some householdB
                Name = "Theo Sun"
                Location = ActorAtPlace homeA
                CurrentActivity = ActorIdle
                Control = AiControlled
                Health = Healthy
                LegalStatus = NoLegalConcern
                Heat = NoHeat
                Reputation = 0.50
                Relationships = Map.empty
                Memories = []
                Inventory = Map.empty
                CurrentVehicle = None
                ActiveTrip = None }
              scenarioActor,
              { Id = scenarioActor
                PersonId = None
                HouseholdId = None
                Name = "Opportunistic NPC"
                Location = ActorAtPlace generalStore
                CurrentActivity = ActorShopping
                Control = AiControlled
                Health = Healthy
                LegalStatus = NoLegalConcern
                Heat = NoHeat
                Reputation = 0.35
                Relationships = Map.empty
                Memories = []
                Inventory = Map.empty
                CurrentVehicle = None
                ActiveTrip = None } ]
            |> Map.ofList

        let streetBuildings =
            [ let shopBuilding = BuildingId(parcelIdNamed "Main Street Retail")
              shopBuilding,
              { Id = shopBuilding
                Place = generalStore
                Name = "Main Street Goods"
                Access = BuildingPublic
                Neighborhood = Some downtown
                IsOpen = true
                Condition = 0.82 }
              let residenceBuilding = BuildingId(parcelIdNamed "Canal Apartments Block")
              residenceBuilding,
              { Id = residenceBuilding
                Place = homeB
                Name = "Canal Apartments"
                Access = BuildingPrivateResidence
                Neighborhood = Some downtown
                IsOpen = true
                Condition = 0.64 }
              let policeBuilding = BuildingId(parcelIdNamed "Civic Office Row")
              policeBuilding,
              { Id = policeBuilding
                Place = policeStation
                Name = "Small Police Station"
                Access = BuildingRestrictedInstitution
                Neighborhood = Some downtown
                IsOpen = true
                Condition = 0.76 } ]
            |> Map.ofList

        let link a b =
            [ a, Set.singleton b; b, Set.singleton a ]

        let streetConnections =
            [ yield! link homeA homeB
              yield! link homeB grocer
              yield! link grocer generalStore
              yield! link generalStore policeStation
              yield! link generalStore office
              yield! link generalStore elementary
              yield! link generalStore workshop ]
            |> List.groupBy fst
            |> List.map (fun (place, links) -> place, links |> List.collect (snd >> Set.toList) |> Set.ofList)
            |> Map.ofList

        let street =
            { Actors = streetActors
              Vehicles =
                [ parkedVehicle,
                  { Id = parkedVehicle
                    Name = "parked compact car"
                    Location = ActorAtPlace homeA
                    Access = VehicleLocked
                    Controller = None
                    Disabled = false
                    Damage = 0.0
                    CurrentTrip = None } ]
                |> Map.ofList
              Buildings = streetBuildings
              PlaceConnections = streetConnections
              ActiveAreas =
                [ { Center = ActorAtPlace homeA
                    RadiusMeters = 350.0
                    DetailLevel = StreetScale } ]
              Dispatches = Map.empty
              RecentEventIds = [] }

        let emptyIndexes =
            { PersonIdsByHousehold = Map.empty
              PersonIdsByNeighborhood = Map.empty
              RelationshipIdsByPerson = Map.empty
              GroupIdsByPerson = Map.empty
              UnitIdsByNeighborhood = Map.empty
              InstitutionIdsByNeighborhood = Map.empty
              StudentIdsBySchool = Map.empty }

        let emptyRuntime =
            { PersonIndexById = Map.empty
              PersonIdsByIndex = [||]
              HouseholdIndexById = Map.empty
              HouseholdIdsByIndex = [||]
              LaneIndexById = Map.empty
              LaneIdsByIndex = [||]
              NeedsByPersonIndex = [||]
              RelationshipsByPersonIndex = [||]
              LanesByIndex = [||]
              IntersectionIncomingLaneRanges = Map.empty
              TripsByPartition = Map.empty
              RouteCache = Map.empty
              TravelTimeCache = Map.empty
              CacheVersion = 0 }

        let performance =
            { MaxCandidateActionsPerPersonPerTick = 3
              MaxSocialInteractionsConsideredPerPersonPerDay = 8
              MaxMemoriesInspectedPerDecision = 12
              MaxRouteAlternativesPerTrip = 3
              MaxReroutesPerTrip = 2
              MaxInstitutionsConsideredPerRequest = 4
              MaxSearchRadiusMeters = 5000.0
              MaxEventSummarySizePerTick = 64
              MaxActiveTripsPerPartitionBeforeAggregation = 128 }

        let performanceDiagnostics =
            { PhaseDiagnostics = []
              AgentsProcessed = 0
              TripsProcessed = 0
              IntentsGenerated = 0
              EventsEmitted = 0
              RouteCalculations = 0
              CacheHits = 0
              CacheMisses = 0
              FullScanWarnings = []
              MemoryCompactions = 0
              EventLogCompactions = 0
              PartitionWorkloads = Map.empty }

        let generationReport = WorldGeneration.placeholderReport 1337 OldIndustrialRiverCity

        let world =
            { Day = 5
              MinuteOfDay = 6 * 60
              Geography = geography
              Region = region
              Settlements = settlements
              Districts = districts
              Blocks = blocks
              GeneratedJobs = generatedJobs
              Sims = [ simA.Id, simA; simB.Id, simB; child.Id, child ] |> Map.ofList
              Households = households
              Relationships = relationships
              Groups = groups
              Institutions = institutions
              Neighborhoods = neighborhoods
              HousingUnits = housingUnits
              Memories = Map.empty
              Street = street
              Transport = transport
              Runtime = emptyRuntime
              Performance = performance
              PerformanceDiagnostics = performanceDiagnostics
              Map =
                { Places = places |> List.map (fun place -> place.Id, place) |> Map.ofList
                  RoadNodes = roadNodeMap
                  RoadSegments = roadSegments
                  MetersPerMapUnit = 500.0 }
              City = city
              Diagnostics = { OverallFragility = 0.0; Risks = [] }
              GenerationReport = generationReport
              Meta =
                { Seed = 1337
                  Tick = 0
                  EventLog = []
                  Decisions = []
                  Indexes = emptyIndexes } }

        let world =
            { world with
                Meta = { world.Meta with Indexes = SimulationPipeline.rebuildIndexes world }
                Runtime = SimulationPipeline.rebuildRuntimeIndexes world }

        let world =
            { world with GenerationReport = WorldGeneration.refreshReport world }

        { world with Diagnostics = Diagnostics.tick world }

    let createWorld seed =
        let world = createSampleWorld ()

        { world with
            GenerationReport = { world.GenerationReport with Seed = seed }
            Meta = { world.Meta with Seed = seed } }
