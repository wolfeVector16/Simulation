using System.Collections.Generic;
using Microsoft.FSharp.Collections;

namespace RealSim.Avalonia.Services;

internal static class FSharpInterop
{
    public static IEnumerable<(TKey Key, TValue Value)> Pairs<TKey, TValue>(FSharpMap<TKey, TValue> map)
        where TKey : notnull
    {
        foreach (var item in map)
        {
            yield return (item.Key, item.Value);
        }
    }
}
