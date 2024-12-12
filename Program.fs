// For more information see https://aka.ms/fsharp-console-apps

open System.IO

let main =
    let x = File.ReadAllText("input/day1.txt")

    let svar1 = aoc2024_fsharp.Day2.part1 x
    printfn $"Svar del 1: {svar1}"
    let svar2 = aoc2024_fsharp.Day2.part2 x
    printfn $"Svar del 2: {svar2}"
