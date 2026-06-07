using System;
using System.Collections.Generic;
using System.Linq;
using RealSim.Scenarios;
using SimDomain = Simulation.Domain;

namespace RealSim.Avalonia.Services;

public sealed class SimulationSession : IDisposable
{
    private int _seed = 1337;

    public SimDomain.World CurrentWorld { get; private set; } = Juniper.createWorld(1337);

    public void LoadJuniper(int seed)
    {
        _seed = seed;
        CurrentWorld = Juniper.createWorld(seed);
    }

    public void Reset()
    {
        LoadJuniper(_seed);
    }

    public const int MinutesPerTick = 1;
    public const int MinutesPerDay = 24 * 60;

    public SimDomain.TickResult AdvanceTick()
    {
        return AdvanceMinutes(MinutesPerTick);
    }

    public SimDomain.TickResult AdvanceTick(int minutes)
    {
        return AdvanceMinutes(minutes);
    }

    public SimDomain.TickResult AdvanceMinutes(int minutes)
    {
        var result = Simulation.Engine.tickWithResult(Math.Max(1, minutes), CurrentWorld);
        CurrentWorld = result.Item1;
        return result.Item2;
    }

    public IReadOnlyList<SimDomain.TickResult> AdvanceDay()
    {
        return AdvanceManyMinutes(MinutesPerDay);
    }

    public IReadOnlyList<SimDomain.TickResult> AdvanceDay(int minutesPerStep)
    {
        return AdvanceManyMinutes(MinutesPerDay, Math.Max(1, minutesPerStep));
    }

    public IReadOnlyList<SimDomain.TickResult> AdvanceManyMinutes(int totalMinutes, int minutesPerStep = 1)
    {
        var remaining = Math.Max(1, totalMinutes);
        var step = Math.Max(1, minutesPerStep);
        var results = new List<SimDomain.TickResult>((remaining + step - 1) / step);

        while (remaining > 0)
        {
            var minutes = Math.Min(step, remaining);
            results.Add(AdvanceMinutes(minutes));
            remaining -= minutes;
        }

        return results;
    }

    public IReadOnlyList<string> RecentEvents(int count)
    {
        return CurrentWorld.Meta.EventLog
            .Take(count)
            .Select(EventFormatter.Format)
            .ToArray();
    }

    public void Dispose()
    {
    }
}
