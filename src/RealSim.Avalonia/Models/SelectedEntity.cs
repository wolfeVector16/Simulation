using System.Collections.Generic;

namespace RealSim.Avalonia.Models;

public abstract record SelectedEntity
{
    public sealed record None : SelectedEntity;

    public sealed record Neighborhood(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Road(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Place(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Building(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Institution(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Household(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;

    public sealed record Sim(string Id, string Name, IReadOnlyList<string> Details) : SelectedEntity;
}
