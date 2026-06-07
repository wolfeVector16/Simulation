using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Markup.Xaml;
using RealSim.Avalonia.Services;
using RealSim.Avalonia.ViewModels;

namespace RealSim.Avalonia;

public sealed partial class App : Application
{
    public override void Initialize()
    {
        AvaloniaXamlLoader.Load(this);
    }

    public override void OnFrameworkInitializationCompleted()
    {
        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            var session = new SimulationSession();
            desktop.MainWindow = new MainWindow
            {
                DataContext = new MainWindowViewModel(session)
            };
        }

        base.OnFrameworkInitializationCompleted();
    }
}
