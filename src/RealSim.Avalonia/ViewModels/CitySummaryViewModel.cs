using CommunityToolkit.Mvvm.ComponentModel;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class CitySummaryViewModel : ObservableObject
{
    [ObservableProperty]
    private string name = "No city loaded";

    [ObservableProperty]
    private string populationText = "";

    [ObservableProperty]
    private string budgetText = "";

    [ObservableProperty]
    private string demandText = "";

    [ObservableProperty]
    private string diagnosticsText = "";

    public void Update(CitySummary summary)
    {
        Name = summary.Name;
        PopulationText = summary.PopulationText;
        BudgetText = summary.BudgetText;
        DemandText = summary.DemandText;
        DiagnosticsText = summary.DiagnosticsText;
    }
}
