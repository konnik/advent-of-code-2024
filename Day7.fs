module aoc2024_fsharp.Day7

open System
open FParsec

type Equation =
    { TestValue: int64
      Numbers: int64 list }

let parseInput (input: string) : Equation list =
    let lineP: Parser<Equation, Unit> =
        pipe2 (pint64 .>> skipChar ':' .>> skipChar ' ') (sepBy1 pint64 (skipChar ' ')) (fun a b ->
            { TestValue = a; Numbers = b })

    match run (sepBy1 lineP (skipNewline)) input with
    | Success(result, _, _) -> result
    | Failure(err, _, _) -> failwith $"Parse error: {err}"


let hasSolution1 (equation: Equation) : bool =
    let rec search acc numbers =
        if acc > equation.TestValue then
            false
        else
            match numbers with
            | [] -> acc = equation.TestValue
            | x :: xs -> search (acc * x) xs || search (acc + x) xs

    match equation.Numbers with
    | [] -> failwith "Illegal equation"
    | x :: xs -> search x xs


let part1 (input: string) : string =
    let equations = parseInput input
    let answer = equations |> List.filter hasSolution1 |> List.sumBy (_.TestValue)

    answer.ToString()


let hasSolution2 (equation: Equation) : bool =
    let concat a b =
        Int64.Parse(a.ToString() + b.ToString())

    let rec search acc numbers =
        if acc > equation.TestValue then
            false
        else
            match numbers with
            | [] -> acc = equation.TestValue
            | x :: xs -> search (acc * x) xs || search (acc + x) xs || search (concat acc x) xs

    match equation.Numbers with
    | [] -> failwith "Illegal equation"
    | x :: xs -> search x xs


let part2 (input: string) : string =
    let equations = parseInput input

    let answer =
        equations
        |> List.filter (fun e -> hasSolution1 e || hasSolution2 e)
        |> List.sumBy (_.TestValue)

    answer.ToString()
