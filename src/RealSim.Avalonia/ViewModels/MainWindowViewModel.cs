using System;
using System.Collections.ObjectModel;
using System.Linq;
using Avalonia.Threading;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using RealSim.Avalonia.Services;
using SimDomain = Simulation.Domain;

namespace RealSim.Avalonia.ViewModels;

public sealed partial class MainWindowViewModel : ObservableObject
{
    private readonly SimulationSession _session;
    private readonly DispatcherTimer _playTimer;
    private readonly int _seed = 1337;
    private readonly int _tickMinutes = 15;

    [ObservableProperty]
    private MapViewModel map = new();

    [ObservableProperty]
    private CitySummaryViewModel citySummary = new();

    [ObservableProperty]
    private TransportSummaryViewModel transportSummary = new();

    [ObservableProperty]
    private SelectedEntityViewModel selectedEntity = new();

    [ObservableProperty]
    private string statusText = "Loading...";

    [ObservableProperty]
    private bool isPlaying;

    public string PlayPauseText => IsPlaying ? "Pause" : "Play";

    public ObservableCollection<EventLogItemViewModel> RecentEvents { get; } = new();

    public ObservableCollection<NeighborhoodSummaryViewModel> Neighborhoods { get; } = new();

    public MainWindowViewModel(SimulationSession session)
    {
        _session = session;
        Map.SelectionChanged += primitive => SelectedEntity.Show(primitive);
        Map.MovingSelectionChanged += entity => SelectedEntity.Show(entity);
        _playTimer = new DispatcherTimer { Interval = TimeSpan.FromMilliseconds(1200) };
        _playTimer.Tick += (_, _) => AdvanceTick();
        LoadJuniperScenario();
    }

    partial void OnIsPlayingChanged(bool value)
    {
        OnPropertyChanged(nameof(PlayPauseText));
        if (value)
        {
            _playTimer.Start();
        }
        else
        {
            _playTimer.Stop();
        }
    }

    [RelayCommand]
    private void LoadJuniperScenario()
    {
        try
        {
            _session.LoadJuniper(_seed);
            Map.ResetView();
            RefreshFromWorld();
            StatusText = $"Juniper loaded: day {_session.CurrentWorld.Day}, {Simulation.Measures.formatTime(_session.CurrentWorld.MinuteOfDay)}";
        }
        catch (Exception ex)
        {
            StatusText = $"Failed to load Juniper: {ex.Message}";
        }
    }

    [RelayCommand]
    private void AdvanceTick()
    {
        try
        {
            _ = _session.AdvanceTick(_tickMinutes);
            RefreshFromWorld();
            StatusText = $"Advanced {_tickMinutes} minutes: day {_session.CurrentWorld.Day}, {Simulation.Measures.formatTime(_session.CurrentWorld.MinuteOfDay)}";
        }
        catch (Exception ex)
        {
            StatusText = $"Advance failed: {ex.Message}";
        }
    }

    [RelayCommand]
    private void AdvanceDay()
    {
        try
        {
            _ = _session.AdvanceDay(_tickMinutes);
            RefreshFromWorld();
            StatusText = $"Advanced one day: day {_session.CurrentWorld.Day}, {Simulation.Measures.formatTime(_session.CurrentWorld.MinuteOfDay)}";
        }
        catch (Exception ex)
        {
            StatusText = $"Advance day failed: {ex.Message}";
        }
    }

    [RelayCommand]
    private void TogglePlayPause()
    {
        IsPlaying = !IsPlaying;
        StatusText = IsPlaying ? "Playing simulation ticks." : "Paused.";
    }

    [RelayCommand]
    private void ResetScenario()
    {
        _session.Reset();
        Map.ResetView();
        RefreshFromWorld();
        StatusText = "Scenario reset.";
    }

    [RelayCommand]
    private void ZoomIn()
    {
        Map.ZoomBy(1.18);
    }

    [RelayCommand]
    private void ZoomOut()
    {
        Map.ZoomBy(1.0 / 1.18);
    }

    [RelayCommand(CanExecute = nameof(CanUseCityCommandHook))]
    private void BuildBuilding()
    {
        StatusText = "Build Building hook is ready for command submission; select a parcel in a later tool pass.";
    }

    [RelayCommand(CanExecute = nameof(CanUseCityCommandHook))]
    private void DestroyBuilding()
    {
        StatusText = "Destroy Building hook is ready for command submission; consequences must flow through Engine.advanceTick.";
    }

    [RelayCommand(CanExecute = nameof(CanUseCityCommandHook))]
    private void RezoneParcel()
    {
        StatusText = "Rezone Parcel hook is ready for command preview/apply; no UI mutation path exists.";
    }

    [RelayCommand(CanExecute = nameof(CanUseCityCommandHook))]
    private void BuildRoad()
    {
        StatusText = "Build Road hook is ready for command preview/apply; no UI mutation path exists.";
    }

    private bool CanUseCityCommandHook() => false;

    private void RefreshFromWorld()
    {
        var world = _session.CurrentWorld;
        Map.Update(MapProjection.Project(world));
        CitySummary.Update(CreateCitySummary(world));
        TransportSummary.Update(CreateTransportSummary(world));
        RefreshNeighborhoods(world);
        RefreshEvents();
        if (Map.SelectedMovingEntity is not null)
        {
            SelectedEntity.Show(Map.SelectedMovingEntity);
        }
        else
        {
            SelectedEntity.Show(Map.SelectedPrimitive);
        }
    }

    private void RefreshEvents()
    {
        RecentEvents.Clear();
        foreach (var item in _session.RecentEvents(30))
        {
            RecentEvents.Add(new EventLogItemViewModel(item));
        }
    }

    private void RefreshNeighborhoods(SimDomain.World world)
    {
        Neighborhoods.Clear();
        foreach (var item in FSharpInterop.Pairs(world.Neighborhoods).OrderBy(item => item.Value.Name))
        {
            var neighborhood = item.Value;
            Neighborhoods.Add(new NeighborhoodSummaryViewModel(
                neighborhood.Name,
                $"residents={neighborhood.Residents.Count}, land={neighborhood.LandValue:0.00}, safety={neighborhood.Safety:0.00}, transit={neighborhood.TransitAccess:0.00}"));
        }
    }

    private static RealSim.Avalonia.Models.CitySummary CreateCitySummary(SimDomain.World world)
    {
        var city = world.City;
        var indicators = city.Indicators;
        return new RealSim.Avalonia.Models.CitySummary(
            city.Name,
            $"population={indicators.Population}, jobs={indicators.Jobs}, unemployment={indicators.Unemployment:P0}",
            $"treasury={city.Budget.Treasury:0}, income={city.Budget.MonthlyIncome:0}, expenses={city.Budget.MonthlyExpenses:0}",
            $"RCI R={city.Demand.Residential:0.00} C={city.Demand.Commercial:0.00} I={city.Demand.Industrial:0.00}",
            $"fragility={world.Diagnostics.OverallFragility:0.00}, events={world.Meta.EventLog.Length}, memories={world.Memories.Count}");
    }

    private static RealSim.Avalonia.Models.TransportSummary CreateTransportSummary(SimDomain.World world)
    {
        var metrics = world.Transport.Metrics;
        return new RealSim.Avalonia.Models.TransportSummary(
            $"roads={world.Map.RoadSegments.Length}, lanes={world.Transport.Lanes.Count}, trips={world.Transport.Trips.Count}",
            $"reliability={metrics.AverageTravelReliability:0.00}, congestion={metrics.AverageCongestion:0.00}, transitTrust={metrics.TransitTrust:0.00}",
            $"late={metrics.LateArrivalsToday}, failedMerges={metrics.FailedLaneChangesToday}, parkingFailures={metrics.ParkingFailuresToday}");
    }
}
