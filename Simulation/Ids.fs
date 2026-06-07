namespace Simulation.Domain

open System

type SimId = SimId of Guid
type PlaceId = PlaceId of Guid
type HouseholdId = HouseholdId of Guid
type HouseholdObjectId = HouseholdObjectId of Guid
type EventId = EventId of Guid
type MemoryId = MemoryId of Guid
type RelationshipId = RelationshipId of Guid
type GroupId = GroupId of Guid
type InstitutionId = InstitutionId of Guid
type PlayerId = PlayerId of Guid
type ScenarioId = ScenarioId of Guid
type DisasterId = DisasterId of Guid
type SettlementId = SettlementId of Guid
type DistrictId = DistrictId of Guid
type BlockId = BlockId of Guid
type JobId = JobId of Guid
type NeighborhoodId = NeighborhoodId of Guid
type RoadNodeId = RoadNodeId of Guid
type RoadSegmentId = RoadSegmentId of Guid
type LotId = LotId of Guid
type UnitId = UnitId of Guid
type LaneId = LaneId of Guid
type SignalPlanId = SignalPlanId of Guid
type TransitRouteId = TransitRouteId of Guid
type TransitStopId = TransitStopId of Guid
type TransportTripId = TransportTripId of Guid
type TransportRouteId = TransportRouteId of Guid
type VehicleId = VehicleId of Guid
type ActorId = ActorId of Guid
type ItemId = ItemId of Guid
type InteractionId = InteractionId of Guid
type RoomId = RoomId of Guid
type PoliceDispatchId = PoliceDispatchId of Guid
type ParkingZoneId = ParkingZoneId of Guid
type TransportIncidentId = TransportIncidentId of Guid
[<Struct>]
type SimulationSeed = SimulationSeed of int
[<Struct>]
type TickId = TickId of int
[<Struct>]
type PartitionId = PartitionId of string
type ParcelId = ParcelId of Guid
type BuildingId = BuildingId of ParcelId
