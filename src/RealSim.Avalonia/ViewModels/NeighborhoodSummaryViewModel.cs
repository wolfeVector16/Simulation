namespace RealSim.Avalonia.ViewModels;

public sealed class NeighborhoodSummaryViewModel
{
    public NeighborhoodSummaryViewModel(string name, string summary)
    {
        Name = name;
        Summary = summary;
    }

    public string Name { get; }

    public string Summary { get; }
}
