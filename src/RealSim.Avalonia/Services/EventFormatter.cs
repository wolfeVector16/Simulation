using System;
using SimDomain = Simulation.Domain;

namespace RealSim.Avalonia.Services;

public static class EventFormatter
{
    public static string Format(SimDomain.DomainEvent domainEvent)
    {
        var text = domainEvent.ToString();
        return string.IsNullOrWhiteSpace(text) ? domainEvent.GetType().Name : text;
    }
}
