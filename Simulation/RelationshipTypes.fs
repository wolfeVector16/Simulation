namespace Simulation.Domain

open System

type RelationshipKind =
    | ParentOf
    | ChildOf
    | SiblingOf
    | SpouseOf
    | ExPartnerOf
    | FriendOf
    | CoworkerOf
    | ClassmateOf
    | NeighborOf
    | EmployerOf
    | EmployeeOf
    | TeacherOf
    | StudentOf
    | LandlordOf
    | TenantOf
    | CaregiverOf
    | DependentOf
    | RivalOf
    | DebtorTo
    | CreditorOf
    | ServiceProviderFor
    | CommunityMemberWith
type TieStrength =
    | CloseTie
    | RegularTie
    | WeakTie
    | KnownByReputation
    | Stranger
type RelationshipDimensions =
    { Affection: float
      Trust: float
      Attraction: float
      Respect: float
      Fear: float
      Obligation: float
      Dependence: float
      Resentment: float
      Familiarity: float
      PowerImbalance: float
      Loyalty: float
      Reputation: float
      Conflict: float }
type RelationshipEdge =
    { Id: RelationshipId
      From: SimId
      Toward: SimId
      Kinds: Set<RelationshipKind>
      Strength: TieStrength
      Dimensions: RelationshipDimensions
      LastInteractionDay: int option }
