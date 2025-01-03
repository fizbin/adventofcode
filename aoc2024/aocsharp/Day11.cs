using System;
using System.Collections.Generic;
using System.IO;
using System.Numerics;

public class Day11
{
    private List<Dictionary<Int64, BigInteger>> precomputed;

    BigInteger PebbleCount(Int64 pebble, int iterCount)
    {
        if (pebble < 0)
        {
            throw new ArgumentOutOfRangeException("pebble", "Invalid pebble value: " + pebble.ToString());
        }
        if (iterCount < 1)
        {
            return 1;
        }
        if ((iterCount < precomputed.Count) && precomputed[iterCount].TryGetValue(pebble, out var pcAns))
        {
            return pcAns;
        }
        //Console.WriteLine(pebble);
        if (pebble == 0)
        {
            return PebbleCount(1, iterCount - 1);
        }
        else
        {
            string repr = pebble.ToString();
            if (repr.Length % 2 == 0)
            {
                try
                {
                    long n1 = Int64.Parse(repr.Substring(0, repr.Length / 2));
                    long n2 = Int64.Parse(repr.Substring(repr.Length / 2));
                    var sub1 = PebbleCount(n1, iterCount - 1);
                    var sub2 = PebbleCount(n2, iterCount - 1);
                    return sub1 + sub2;
                }
                catch (FormatException)
                {
                    throw new InvalidOperationException(repr);
                }
            }
            return PebbleCount(pebble * 2024, iterCount - 1);
        }
    }

    void BuildPrecompute(int iterationLimit, params Int64[] pebbleNums)
    {
        while (precomputed.Count <= iterationLimit)
        {
            Dictionary<Int64, BigInteger> cache = new Dictionary<Int64, BigInteger>(pebbleNums.Length);
            for (int idx = 0; idx < pebbleNums.Length; idx++)
            {
                cache.Add(pebbleNums[idx], PebbleCount(pebbleNums[idx], precomputed.Count));
            }
            precomputed.Add(cache);
        }
    }

    long[] ParseInput(string inFile)
    {
        string data = File.ReadAllText(inFile);
        var split = data.Split(' ');
        var ans = new long[split.Length];
        for (int i = 0; i < split.Length; i++)
        {
            ans[i] = Int64.Parse(split[i]);
        }
        return ans;
    }

    public Day11()
    {
        precomputed = new List<Dictionary<Int64, BigInteger>>(75);
    }

    public static void Run(string inFile)
    {
        Day11 day11 = new Day11();
        day11.BuildPrecompute(75, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 20, 24);
        var data = day11.ParseInput(inFile);
        BigInteger ans = 0;
        for (int idx = 0; idx < data.Length; idx++)
        {
            ans += day11.PebbleCount(data[idx], 25);
        }
        Console.WriteLine("Part 1: " + ans.ToString());
        ans = 0;
        for (int idx = 0; idx < data.Length; idx++)
        {
            ans += day11.PebbleCount(data[idx], 75);
        }
        Console.WriteLine("Part 2: " + ans.ToString());
    }
}
