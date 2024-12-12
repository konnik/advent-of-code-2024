module aoc2024_fsharp.Day2

open aoc2024_fsharp.Util


type Report = int list

let parseReport (line : string) : Report =
    words line |> List.map parseInt
let parseInput (input: string) : Report list =
    lines input
    |> List.map parseReport
    
type Classification = Safe | Unsafe

let increasing (a:int, b: int) : bool =
    a < b

let decreasing (a:int, b: int) : bool =
    a > b


let differBy (atLeast: int) (atMost:int) (a:int, b: int) : bool =
    let d = abs (b - a)
    d>= atLeast && d<= atMost
    

let classify (report:Report) : Classification =
    let pairs = List.pairwise report
    let allIncreasing = List.forall increasing pairs
    let allDecreasing = List.forall decreasing pairs
    let allDiffsOk = List.forall (differBy 1 3) pairs
    
    if (allIncreasing || allDecreasing) && allDiffsOk then
        Safe
     else
         Unsafe
    

let part1 (input: string) : string = 
    let reports = parseInput input
    
    let numberOfSafeReports =
        reports
        |> List.map classify
        |> List.filter ((=) Safe)
        |> List.length
    
    numberOfSafeReports.ToString()
    
    
let rec dampenedReports (report: Report): Report list =
    let rec go a b = 
        match b with
        | [] -> []
        | x::rest ->
            let c = List.append (List.rev a) (rest)
            c :: go (x::a) rest
            
    go [] report
    
let candidates (report: Report) : Report list=
    report :: dampenedReports report
    
let part2 (input: string) : string = 
    let reports = parseInput input
    
    let numberOfSafeReports =
        reports
        |> List.map candidates
        |> List.map (List.map classify)
        |> List.filter (List.exists ((=) Safe))
        |> List.length
    
    numberOfSafeReports.ToString()

