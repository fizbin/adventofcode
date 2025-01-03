using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace aocsharp
{
    internal class Program
    {
        static void Main(string[] args)
        {
            int dayNum=0;
            if (args.Length < 1 || !Int32.TryParse(args[0], out dayNum))
            {
                Console.WriteLine("ERROR! Please give a first argument that is a day number (e.g. 11)");
                Environment.Exit(1);
            }
            if (!((dayNum >= 1) && (dayNum <= 25))) {
                Console.WriteLine("ERROR! Day number must be an integer from 1 to 25");
                Environment.Exit(1);
            }
            String infile = "..\\aoc" + args[0] + ".in";
            if (args.Length == 2)
            {
                infile = args[1];
            }
            switch (dayNum)
            {
                case 11:
                    Day11.Run(infile);
                    break;
                default:
                    Console.WriteLine("Error: Day " + dayNum + " not implemented yet.");
                    Environment.Exit(1);
                    break;
            }
        }
    }
}
