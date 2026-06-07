namespace Simulation.Domain

open System

[<Struct>]
type SimTime =
    { Day: int
      MinuteOfDay: int }
