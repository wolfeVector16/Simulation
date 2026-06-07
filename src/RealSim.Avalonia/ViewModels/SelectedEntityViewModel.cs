using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using RealSim.Avalonia.Models;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class SelectedEntityViewModel : ObservableObject
{
    [ObservableProperty]
    private string title = "Nothing selected";

    [ObservableProperty]
    private string subtitle = "Click the map to inspect roads, parcels, institutions, and households.";

    public ObservableCollection<string> Details { get; } = new();

    public void Show(MapPrimitive? primitive)
    {
        Details.Clear();

        if (primitive is null)
        {
            Title = "Nothing selected";
            Subtitle = "Click the map to inspect roads, parcels, institutions, and households.";
            return;
        }

        Title = primitive.Name;
        Subtitle = $"{ReadableKind(primitive.Kind)}  {primitive.Id}";
        Details.Add($"Category: {ReadableText(primitive.Category)}");
        Details.Add(primitive.Details);
        if (primitive.IsApproximate)
        {
            Details.Add("Route/location is approximate because the engine did not expose exact route geometry.");
        }

        Details.Add($"Drawn points: {primitive.Points.Count}");
    }

    public void Show(MovingEntityViewModel? entity)
    {
        Details.Clear();

        if (entity is null)
        {
            Show((MapPrimitive?)null);
            return;
        }

        Title = entity.DisplayName;
        Subtitle = $"{ReadableText(entity.Kind.ToString())}  {entity.Id}";
        Details.Add($"Mode: {entity.Mode}");
        Details.Add($"Purpose: {entity.Purpose}");
        Details.Add($"Status: {entity.Status}");
        Details.Add($"Progress: {entity.Progress:P0}");
        Details.Add($"Route points: {entity.RoutePolyline.Count}");
        if (entity.IsApproximate)
        {
            Details.Add("Route is approximate: drawn from origin/destination or partial route data.");
        }
    }

    private static string ReadableKind(MapPrimitiveKind kind)
    {
        return ReadableText(kind.ToString());
    }

    private static string ReadableText(string text)
    {
        if (string.IsNullOrWhiteSpace(text))
        {
            return text;
        }

        var result = text[0].ToString();
        for (var i = 1; i < text.Length; i++)
        {
            if (char.IsUpper(text[i]) && !char.IsWhiteSpace(text[i - 1]))
            {
                result += " ";
            }

            result += text[i];
        }

        return result;
    }
}
