namespace Simulation.Domain

open System

type Trait =
    | Neat
    | Slob
    | Romantic
    | Genius
    | Creative
    | HotHeaded
    | Lazy
    | Outgoing
    | Loner
    | FamilyOriented
    | Ambitious
type SkillKind =
    | Cooking
    | Charisma
    | Logic
    | Fitness
    | Painting
    | Writing
    | Music
    | Handiness
    | Gardening
    | Programming
    | Creativity
type Skill =
    { Level: int
      Experience: float }
