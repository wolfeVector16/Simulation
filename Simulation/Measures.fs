namespace Simulation

module Measures =
    let clamp01 value =
        value |> max 0.0 |> min 1.0
    let minutesPerDay = 24 * 60
    let normalizeMinute minute =
        let wrapped = minute % minutesPerDay
        if wrapped < 0 then wrapped + minutesPerDay else wrapped
    let formatTime minute =
        let normalized = normalizeMinute minute
        let hour = normalized / 60
        let mins = normalized % 60
        sprintf "%02i:%02i" hour mins
