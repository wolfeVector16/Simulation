namespace Simulation.Domain

open System

[<Struct>]
type Money = Money of decimal
[<Struct>]
type Minutes = Minutes of int
[<Struct>]
type Meters = Meters of float
[<Struct>]
type MetersPerSecond = MetersPerSecond of float
[<Struct>]
type VehiclesPerHour = VehiclesPerHour of float
[<Struct>]
type Capacity = Capacity of int
[<Struct>]
type Probability = private Probability of float
[<Struct>]
type Score = private Score of float
module Quantities =
    let probability value =
        if value < 0.0 || value > 1.0 then
            Error "Probability must be between 0 and 1."
        else
            Ok(Probability value)
    let score value =
        if value < 0.0 || value > 1.0 then
            Error "Score must be between 0 and 1."
        else
            Ok(Score value)
    let probabilityValue (Probability value) = value
    let scoreValue (Score value) = value
