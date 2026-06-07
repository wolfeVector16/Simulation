namespace RealSim.Avalonia.ViewModels;

public sealed class EventLogItemViewModel
{
    public EventLogItemViewModel(string text)
    {
        Text = text;
    }

    public string Text { get; }
}
