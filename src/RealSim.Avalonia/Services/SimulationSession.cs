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

    public SimDomain.TickResult AdvanceTick(int tickMinutes)
    {
        var result = Simulation.Engine.tickWithResult(tickMinutes, CurrentWorld);
        CurrentWorld = result.Item1;
        return result.Item2;
    }

    public IReadOnlyList<SimDomain.TickResult> AdvanceDay(int tickMinutes)
    {
        var steps = Math.Max(1, (24 * 60) / Math.Max(1, tickMinutes));
        var results = new List<SimDomain.TickResult>(steps);

        for (var i = 0; i < steps; i++)
        {
            results.Add(AdvanceTick(tickMinutes));
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
