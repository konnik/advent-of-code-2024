module aoc2024_fsharp.Day1

open aoc2024_fsharp.Util

type Pair = int * int

let parseLine (line: string) : Pair =
    match words line with
    | [ left; right ] -> (parseInt left, parseInt right)
    | _ -> failwith $"Parsefel: {line}"

let parseIput (input: string) : (int list * int list) =
    let pairs = lines input |> List.map parseLine

    let left = pairs |> List.map fst
    let right = pairs |> List.map snd
    (left, right)

let distance (a, b) = abs (a - b)

let part1 (input: string) : string =
    let (left, right) = parseIput input

    let leftSorted = List.sort left
    let rightSorted = List.sort right

    let totalDistance = List.zip leftSorted rightSorted |> List.sumBy distance
    totalDistance.ToString()


let similarityScore (frequencies: Map<int, int>) (id: int) : int =
    let appearances = (Map.tryFind id frequencies |> Option.defaultValue 0)
    id * appearances

let part2 (input: string) : string =
    let (left, right) = parseIput input

    let frequencies = right |> List.countBy id |> Map.ofList

    let totalSimilarityScore = left |> List.sumBy (similarityScore frequencies)
    totalSimilarityScore.ToString()
