using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.FSharp.Reflection;

var assemblyPath = @"D:\source\Simulation\Simulation\bin\Debug\net10.0\Simulation.dll";
var outDir = @"D:\source\Simulation\Simulation";
var asm = Assembly.LoadFile(assemblyPath);
var domain = asm.GetType("Simulation.Domain") ?? throw new Exception("Simulation.Domain not found");
var nested = domain.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic).ToDictionary(t => t.Name.Split('`')[0], t => t);

string TypeName(Type t)
{
    if (t == typeof(int)) return "int";
    if (t == typeof(double)) return "float";
    if (t == typeof(decimal)) return "decimal";
    if (t == typeof(bool)) return "bool";
    if (t == typeof(string)) return "string";
    if (t == typeof(Guid)) return "Guid";
    if (t == typeof(object)) return "obj";
    if (t == typeof(DateTime)) return "DateTime";
    if (FSharpType.IsTuple(t, null)) return string.Join(" * ", FSharpType.GetTupleElements(t).Select(TypeName));
    if (t.IsGenericType)
    {
        var def = t.GetGenericTypeDefinition();
        var args = t.GetGenericArguments().Select(TypeName).ToArray();
        if (def.FullName == "Microsoft.FSharp.Collections.FSharpList`1") return args[0] + " list";
        if (def.FullName == "Microsoft.FSharp.Collections.FSharpMap`2") return $"Map<{args[0]}, {args[1]}>";
        if (def.FullName == "Microsoft.FSharp.Collections.FSharpSet`1") return $"Set<{args[0]}>";
        if (def.FullName == "Microsoft.FSharp.Core.FSharpOption`1") return args[0] + " option";
        if (def.FullName == "Microsoft.FSharp.Core.FSharpChoice`2") return $"Choice<{args[0]}, {args[1]}>";
        var tick = t.Name.IndexOf('`');
        var name = tick >= 0 ? t.Name[..tick] : t.Name;
        return $"{name}<{string.Join(", ", args)}>";
    }
    if (t.DeclaringType == domain) return t.Name;
    if (t.FullName != null && t.FullName.StartsWith("Simulation.Domain+")) return t.Name;
    return t.Name.Replace("System.", "");
}

string UnionDecl(Type t)
{
    var lines = new List<string>();
    if (t.IsValueType) lines.Add("[<Struct>]");
    var privateCase = t.Name is "Probability" or "Score" or "WorldSnapshot";
    lines.Add($"type {t.Name} =");
    foreach (var c in FSharpType.GetUnionCases(t, BindingFlags.Public | BindingFlags.NonPublic))
    {
        var fields = c.GetFields();
        var casePrefix = privateCase && fields.Length > 0 ? "private " : "";
        if (fields.Length == 0)
        {
            lines.Add($"    | {c.Name}");
            continue;
        }
        var parts = fields.Select(f =>
        {
            var name = f.Name;
            var typ = TypeName(f.PropertyType);
            return string.IsNullOrWhiteSpace(name) || name.StartsWith("Item") ? typ : $"{name}: {typ}";
        });
        lines.Add($"    | {casePrefix}{c.Name} of {string.Join(" * ", parts)}");
    }
    return string.Join(Environment.NewLine, lines);
}

string RecordDecl(Type t)
{
    var lines = new List<string>();
    if (t.IsValueType) lines.Add("[<Struct>]");
    lines.Add($"type {t.Name} =");
    var fields = FSharpType.GetRecordFields(t, BindingFlags.Public | BindingFlags.NonPublic);
    for (var i = 0; i < fields.Length; i++)
    {
        var prefix = i == 0 ? "    {" : "     ";
        var suffix = i == fields.Length - 1 ? " }" : "";
        lines.Add($"{prefix} {fields[i].Name}: {TypeName(fields[i].PropertyType)}{suffix}");
    }
    return string.Join(Environment.NewLine, lines);
}

string TypeDecl(string name)
{
    if (!nested.TryGetValue(name, out var t)) throw new Exception($"Missing type {name}");
    if (FSharpType.IsRecord(t, BindingFlags.Public | BindingFlags.NonPublic)) return RecordDecl(t);
    if (FSharpType.IsUnion(t, BindingFlags.Public | BindingFlags.NonPublic)) return UnionDecl(t);
    throw new Exception($"Unsupported type {name}: {t.FullName}");
}

void WriteDomainFile(string file, params string[] names)
{
    var lines = new List<string> { "namespace Simulation.Domain", "", "open System", "" };
    for (var i = 0; i < names.Length; i++)
    {
        if (i > 0) lines.Add("");
        lines.Add(TypeDecl(names[i]));
    }
    File.WriteAllText(Path.Combine(outDir, file), string.Join(Environment.NewLine, lines) + Environment.NewLine);
}

WriteDomainFile("Ids.fs", "SimId", "PlaceId", "HouseholdId", "HouseholdObjectId", "EventId", "MemoryId", "RelationshipId", "GroupId", "InstitutionId", "PlayerId", "ScenarioId", "DisasterId", "SettlementId", "DistrictId", "BlockId", "JobId", "NeighborhoodId", "RoadNodeId", "RoadSegmentId", "LotId", "UnitId", "LaneId", "SignalPlanId", "TransitRouteId", "TransitStopId", "TransportTripId", "TransportRouteId", "VehicleId", "ActorId", "ItemId", "InteractionId", "RoomId", "PoliceDispatchId", "ParkingZoneId", "TransportIncidentId", "SimulationSeed", "TickId", "PartitionId", "ParcelId", "BuildingId");
WriteDomainFile("Measures.fs", "SimTime", "Money", "Minutes", "Meters", "MetersPerSecond", "VehiclesPerHour", "Capacity", "Probability", "Score");
WriteDomainFile("CommonTypes.fs", "EntityId", "RngPurpose", "RngKey");
WriteDomainFile("NeedTypes.fs", "NeedKind", "Need", "GoodKind", "PurchaseIntent", "SimWant");
WriteDomainFile("SkillTypes.fs", "Trait", "SkillKind", "Skill");
WriteDomainFile("SimTypes.fs", "Personality", "LifeStage", "Emotion", "Moodlet", "AspirationKind", "Aspiration", "Fear", "ObjectKind", "ObjectInteractionKind", "HouseholdObject", "QueuedAction", "Job", "SchoolEnrollment", "TravelPurpose", "Trip", "Location", "Activity", "Sim");
WriteDomainFile("PlaceTypes.fs", "Coordinates", "RoadAccess", "PlaceKind", "CommercialOffering", "ProductionRecipe", "PlaceEconomy", "Place");
WriteDomainFile("RoadTypes.fs", "RoadNode", "TravelMode", "RoadClass", "ParkingRule", "BikeFacility", "RoadRestriction", "LaneType", "LaneDirection", "Movement", "Lane", "SignalPhaseKind", "SignalPhase", "IntersectionControl", "Intersection", "RoadSegment");
WriteDomainFile("MapTypes.fs", "CityMap");
WriteDomainFile("GeographyTypes.fs", "TerrainKind", "NaturalFeatureKind", "NaturalFeature", "Geography", "SettlementType", "UrbanArchetype", "Settlement", "District", "Block", "GeneratedJob", "WorldScenario", "WorldGenerationStep", "ValidationIssue", "ValidationStatus", "ValidationFinding", "WorldGenerationReport", "Region");
WriteDomainFile("LandUseTypes.fs", "LandUse");
WriteDomainFile("ZoningTypes.fs", "ZoneType", "Density", "WealthClass", "ZoneConstraint");
WriteDomainFile("BuildingTypes.fs", "BuildingUse", "BuildingStatus", "Building", "Parcel", "UtilityKind", "UtilitySource", "ServiceKind", "ServiceFacility", "BuildingUtilityDemand", "BuildingAccessibilityProfile");
WriteDomainFile("PolicyTypes.fs", "TaxRates", "Budget", "Demand", "Policies", "DisplacementPolicy", "BuildingModification", "CityPolicy", "EmergencyActionKind");
WriteDomainFile("DiagnosticsTypes.fs", "CityIndicators", "AdvisorSeverity", "AdvisorMessage", "CityState", "SimulationRiskArea", "SimulationRisk", "SimulationDiagnostics");
WriteDomainFile("RelationshipTypes.fs", "RelationshipKind", "TieStrength", "RelationshipDimensions", "RelationshipEdge");
WriteDomainFile("SocialGroupTypes.fs", "SocialGroupKind", "SocialNorm", "SocialGroup");
WriteDomainFile("InstitutionTypes.fs", "InstitutionKind", "AccessRule", "InstitutionFailureMode", "Institution", "InstitutionServiceProgram", "InstitutionModification");
WriteDomainFile("HousingTypes.fs", "OwnershipType", "HousingStatus", "LegalHousingStatus", "HousingUnit", "Neighborhood");
WriteDomainFile("StreetSimulationTypes.fs", "SimulationScale", "TickScale", "ActorControl", "HeatLevel", "ActorLegalStatus", "ActorHealthStatus", "ActorActivity", "ActorLocation", "AccessibleArea", "VehicleAccessState", "BuildingAccessState", "ItemCategory", "StreetItem", "ActorMemory", "Actor", "StreetVehicle", "StreetBuilding", "AwarenessLevel", "WitnessReaction", "ActiveSimulationArea", "PoliceDispatch", "StreetSimulationState");
WriteDomainFile("CommandTypes.fs", "SystemCommandSource", "CommandSource", "PlayerAuthorityMode", "EntityRef", "FundingSource", "DestroyBuildingReason", "RoadLaneSpec", "RoadModification", "RoadDestructionReason", "AssetRef", "BuildBuildingCommand", "DestroyBuildingCommand", "ModifyBuildingCommand", "BuildRoadCommand", "ModifyRoadCommand", "DestroyRoadCommand", "BuildInstitutionCommand", "ModifyInstitutionCommand", "CloseInstitutionCommand", "ZoneParcelsCommand", "RezoneParcelsCommand", "DezoneParcelsCommand", "BuildTransitRouteCommand", "ModifyTransitRouteCommand", "RemoveTransitRouteCommand", "BuildUtilityCommand", "ModifyUtilityCommand", "DestroyUtilityCommand", "SetBudgetCommand", "PassPolicyCommand", "RepealPolicyCommand", "IssueBondCommand", "SetTaxRateCommand", "EmergencyActionCommand", "RepairAssetCommand", "CondemnAssetCommand", "CreateDistrictCommand", "ModifyDistrictCommand", "StreetCommandContext", "AttemptResolution", "MoveActorCommand", "InteractWithPersonCommand", "InteractWithObjectCommand", "EnterVehicleCommand", "ExitVehicleCommand", "DriveVehicleCommand", "UnauthorizedVehicleAccessCommand", "EnterBuildingCommand", "ExitBuildingCommand", "UnauthorizedEntryCommand", "PurchaseItemCommand", "TakeItemCommand", "UseItemCommand", "StartConflictCommand", "FleeSceneCommand", "ReportCrimeCommand", "CallEmergencyServiceCommand", "TrespassCommand", "DamagePropertyCommand", "SurrenderToPoliceCommand", "DebugTeleportActorCommand", "StreetCommand", "CityCommand", "CommandRejection", "UnauthorizedContext", "LegalRiskContext", "ConflictRiskContext", "ActionFeasibility", "CommandWarning", "ValidatedCommand", "CommandValidationResult", "CommandPreview", "ResolvedCommand");
WriteDomainFile("ViewTypes.fs", "HeatDto", "ActorDto", "VehicleDto", "PlaceDto", "EventDto", "InteractionPromptDto", "StreetViewQuery", "StreetViewSnapshot");
WriteDomainFile("DecisionTypes.fs", "DecisionReason", "AgentAction", "Decision", "Intent");
WriteDomainFile("TransportTypes.fs", "TransitStop", "TransitRoute", "ParkingZone", "DriverProfile", "TransportTripPurpose", "LocationRef", "IntersectionMovement", "TransportRoute", "TripStatus", "TransportTrip", "VehicleState", "VehiclePosition", "VehicleStatus", "TransportIncidentKind", "TransportIncident", "TransportEvent", "AccessProfile", "TransportMetrics", "VehicleRenderPosition", "VehicleVisualStatus", "VehicleView", "RoadSegmentTrafficView", "IntersectionTrafficView", "TrafficVisualizationEvent", "TrafficFrameMetrics", "RenderableRouteSegment", "RenderableRoute", "VehicleMotionView", "TrafficFrame", "TrafficFrameDiff", "TransportState");
WriteDomainFile("MemoryTypes.fs", "MemorySalience", "MemoryEffect", "Memory");
WriteDomainFile("EventTypes.fs", "DomainEvent");
WriteDomainFile("WorldState.fs", "SnapshotData", "Pressure", "PressureBatch", "IntentBatch", "ResolvedIntent", "ResolvedIntentBatch", "EventBatch", "ChangedState", "PhaseDiagnostic", "TickResult", "TickInput", "Household", "DerivedIndexes", "PersonIndex", "HouseholdIndex", "LaneIndex", "RelationshipIndexRange", "LaneIndexRange", "NeedRuntimeState", "LaneRuntimeState", "CandidateScore", "TripCost", "MovementProposal", "RuntimeIndexes", "SimulationPerformanceBudget", "RuntimePerformanceDiagnostics", "SimulationMeta", "World");

File.AppendAllText(Path.Combine(outDir, "Measures.fs"), @"
module Quantities =
    let probability value =
        if value < 0.0 || value > 1.0 then
            Error ""Probability must be between 0 and 1.""
        else
            Ok(Probability value)

    let score value =
        if value < 0.0 || value > 1.0 then
            Error ""Score must be between 0 and 1.""
        else
            Ok(Score value)

    let probabilityValue (Probability value) = value
    let scoreValue (Score value) = value
");

File.AppendAllText(Path.Combine(outDir, "WorldState.fs"), @"
type WorldSnapshot = private WorldSnapshot of SnapshotData

module WorldSnapshot =
    let create data = WorldSnapshot data
    let value (WorldSnapshot data) = data
");

File.WriteAllText(Path.Combine(outDir, "MeasuresFormatting.fs"), @"namespace Simulation

module Measures =
    let clamp01 value =
        value |> max 0.0 |> min 1.0

    let minutesPerDay = 24 * 60

    let normalizeMinute minute =
        ((minute % minutesPerDay) + minutesPerDay) % minutesPerDay

    let formatTime minute =
        let normalized = normalizeMinute minute
        let hour = normalized / 60
        let minute = normalized % 60
        $""{hour:00}:{minute:00}""
");

Console.WriteLine("Generated domain files.");
