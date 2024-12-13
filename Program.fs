// For more information see https://aka.ms/fsharp-console-apps


open System.IO

let main =
    let input = File.ReadAllText("input/day4.txt")

    let svar1 = aoc2024_fsharp.Day4.part1 input
    printfn $"Svar del 1: {svar1}"

    let svar2 = aoc2024_fsharp.Day4.part2 input
    printfn $"Svar del 2: {svar2}"
