open System.IO
open aoc2024_fsharp

let main =
    let input = File.ReadAllText("input/day5.txt")

    let svar1 = Day5.part1 input
    printfn $"Answer 1: {svar1}"

    let svar2 = Day5.part2 input
    printfn $"Answer 2: {svar2}"
