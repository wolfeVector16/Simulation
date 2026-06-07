namespace Simulation.Domain

open System

type MemorySalience =
    | Trivial
    | Notable
    | Important
    | Traumatic
    | Formative
type MemoryEffect =
    | AffectsTrust of InstitutionId option
    | AffectsFear
    | AffectsResentment of SimId option
    | AffectsLoyalty of SimId option
    | AffectsAvoidance
    | AffectsAmbition
    | AffectsAttachment of PlaceId option
type Memory =
    { Id: MemoryId
      SourceEvent: EventId
      Day: int
      Minute: int
      EmotionalWeight: float
      Salience: MemorySalience
      Tags: Set<string>
      PeopleInvolved: Set<SimId>
      InstitutionsInvolved: Set<InstitutionId>
      Neighborhood: NeighborhoodId option
      Effects: Set<MemoryEffect>
      DecayPerDay: float }
