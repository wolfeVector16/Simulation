namespace Simulation.Domain

open System

type SocialGroupKind =
    | HouseholdGroup
    | FamilyNetwork
    | WorkTeam
    | SchoolClass
    | FriendGroup
    | NeighborhoodBlock
    | CommunityGroup
    | HobbyGroup
    | PoliticalGroup
    | CareNetwork
type SocialNorm =
    | HelpFamily
    | RespectQuietHours
    | ShareChildcare
    | PayDebts
    | AttendMeetings
    | KeepUpAppearances
type SocialGroup =
    { Id: GroupId
      Name: string
      Kind: SocialGroupKind
      Members: Set<SimId>
      SharedNorms: Set<SocialNorm>
      Cohesion: float
      InternalConflict: float
      StatusHierarchy: Map<SimId, float>
      MeetingFrequencyDays: int
      TrustLevel: float
      SharedMemories: MemoryId list
      LocalReputation: float }
