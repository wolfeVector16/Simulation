using CommunityToolkit.Mvvm.ComponentModel;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class TransportSummaryViewModel : ObservableObject
{
    [ObservableProperty]
    private string roadText = "";

    [ObservableProperty]
    private string reliabilityText = "";

    [ObservableProperty]
    private string eventsText = "";

    public void Update(TransportSummary summary)
    {
        RoadText = summary.RoadText;
        ReliabilityText = summary.ReliabilityText;
        EventsText = summary.EventsText;
    }
}
