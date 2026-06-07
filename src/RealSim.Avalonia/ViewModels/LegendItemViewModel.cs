using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed class LegendItemViewModel
{
    public LegendItemViewModel(LegendItem item)
    {
        Label = item.Label;
        Description = item.Description;
        Fill = item.Fill;
        Stroke = item.Stroke;
        Thickness = item.Thickness;
        Symbol = item.Symbol;
        LineStyle = item.LineStyle;
    }

    public string Label { get; }

    public string Description { get; }

    public string Fill { get; }

    public string Stroke { get; }

    public double Thickness { get; }

    public MapSymbol Symbol { get; }

    public MapLineStyle LineStyle { get; }
}
