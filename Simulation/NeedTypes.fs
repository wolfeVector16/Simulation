namespace Simulation.Domain

open System

type NeedKind =
    | Hunger
    | Energy
    | Social
    | Hygiene
    | Fun
    | Bladder
    | Safety
    | Purpose
    | Learning
    | Comfort
    | Environment
type Need =
    { Value: float
      DecayPerHour: float
      CriticalBelow: float }
type Personality =
    { Openness: float
      Conscientiousness: float
      Extraversion: float
      Agreeableness: float
      Neuroticism: float
      Ambition: float
      Frugality: float
      RoutinePreference: float }
