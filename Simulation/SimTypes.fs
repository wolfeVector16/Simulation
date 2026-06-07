namespace Simulation.Domain

open System

type LifeStage =
    | Infant
    | Toddler
    | Child
    | Teen
    | YoungAdult
    | Adult
    | Elder
type GoodKind =
    | Groceries
    | HouseholdGoods
    | Clothing
    | Electronics
    | Entertainment
    | RawMaterials
    | ManufacturedGoods
    | LuxuryGoods
    | Toys
type PurchaseIntent =
    | NeedPurchase
    | WantPurchase
type SimWant =
    { Good: GoodKind
      Desire: float
      WeekendOnly: bool
      MaxTravelMinutes: int }
type Emotion =
    | Fine
    | Happy
    | Sad
    | Angry
    | Inspired
    | Focused
    | Flirty
    | Embarrassed
    | Tense
type Moodlet =
    { Name: string
      Emotion: Emotion
      Strength: float
      RemainingMinutes: int }
type AspirationKind =
    | CareerSuccess
    | BigHappyFamily
    | KnowledgeSeeker
    | SocialButterfly
    | CreativeLife
    | WealthBuilder
type Aspiration =
    { Kind: AspirationKind
      Progress: float
      RewardPoints: int }
type Fear =
    | FearOfFire
    | FearOfFailure
    | FearOfLoneliness
    | FearOfPoverty
    | FearOfBadGrades
type ObjectKind =
    | BedObject
    | FridgeObject
    | StoveObject
    | ShowerObject
    | ToiletObject
    | TvObject
    | ComputerObject
    | EaselObject
    | TreadmillObject
    | BookshelfObject
    | ToyBoxObject
    | SofaObject
    | DecorObject
type ObjectInteractionKind =
    | SleepInBed
    | CookMeal
    | GrabSnack
    | ShowerSelf
    | UseToilet
    | WatchTv
    | PlayGames
    | PracticeSkill of SkillKind
    | CleanObject
    | RepairObject
    | PlayWithToys
    | ReadBook
type HouseholdObject =
    { Id: HouseholdObjectId
      Name: string
      Kind: ObjectKind
      Room: string
      Quality: float
      Cleanliness: float
      Broken: bool
      Interactions: ObjectInteractionKind list }
type QueuedAction =
    { Interaction: ObjectInteractionKind
      Target: HouseholdObjectId option
      Priority: int }
type Job =
    { Title: string
      Workplace: PlaceId
      StartMinute: int
      EndMinute: int
      PayPerDay: decimal }
type SchoolEnrollment =
    { School: PlaceId
      Grade: string
      StartMinute: int
      EndMinute: int
      NeedsEscort: bool }
type TravelPurpose =
    | ToWork
    | ToHome
    | ToErrand
    | ToLeisure
    | ToShopping of PurchaseIntent * GoodKind
    | ToSchool
    | FromSchool
    | ToDaycare
    | FromDaycare
type Trip =
    { Origin: PlaceId
      Destination: PlaceId
      Purpose: TravelPurpose
      RemainingMinutes: int
      TotalMinutes: int }
type Location =
    | AtPlace of PlaceId
    | InTransit of Trip
type Activity =
    | Sleeping
    | MorningRoutine
    | Commuting of TravelPurpose
    | Working
    | Eating
    | Relaxing
    | Socializing
    | Shopping of PurchaseIntent * GoodKind
    | AttendingSchool
    | InDaycare
    | Playing
    | Studying
    | CaringForChild of SimId
    | UsingObject of ObjectInteractionKind
    | Cleaning
    | Repairing
    | PracticingSkill of SkillKind
    | Errand
    | Idle
type Sim =
    { Id: SimId
      Name: string
      LifeStage: LifeStage
      Household: HouseholdId
      Home: PlaceId
      Job: Job option
      School: SchoolEnrollment option
      AgeDays: int
      Traits: Trait list
      Skills: Map<SkillKind, Skill>
      Emotion: Emotion
      Moodlets: Moodlet list
      Aspiration: Aspiration option
      Fears: Fear list
      ActionQueue: QueuedAction list
      Memories: MemoryId list
      SocialCapacity: int
      Needs: Map<NeedKind, Need>
      Personality: Personality
      Location: Location
      Activity: Activity
      Wallet: decimal
      Happiness: float
      Guardians: SimId list
      Dependents: SimId list
      Relationships: Map<SimId, float>
      HouseholdInventory: Map<GoodKind, float>
      Wants: SimWant list }
