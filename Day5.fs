module aoc2024_fsharp.Day5

open FParsec

type Rule = int * int
type Update = int list

let parseInput (input: string) : Rule list * Update list =
    let rules = sepEndBy1 (tuple2 (pint32 .>> skipChar '|') pint32) skipNewline
    let updates = sepEndBy1 (sepBy1 pint32 (skipChar ',')) skipNewline

    let parser = tuple2 (rules .>> skipNewline) updates

    match run parser input with
    | Success(result, _, _) -> result
    | Failure(err, _, _) -> failwith $"Parse error:{err}"


let rec correctOrdering (beforeMap: Map<'a, Set<'a>>) (afterMap: Map<'a, Set<'a>>) (update: 'a list) : bool =
    match update with
    | [] -> true
    | [ _ ] -> true
    | a :: rest ->

        let x =
            rest
            |> List.forall (fun b ->
                match Map.tryFind a afterMap with
                | Some after -> not (Set.contains b after)
                | None -> true)

        let y =
            rest
            |> List.forall (fun b ->
                match Map.tryFind b beforeMap with
                | Some before -> not (Set.contains a before)
                | None -> true)

        x && y && correctOrdering beforeMap afterMap rest


let middle update =
    update |> List.skip (List.length update / 2) |> List.head

let createRuleMaps rules =
    let beforeMap =
        rules
        |> List.groupBy fst
        |> List.map (fun (a, b) -> (a, Set.ofList (List.map snd b)))
        |> Map.ofList

    let afterMap =
        rules
        |> List.map (fun (a, b) -> (b, a))
        |> List.groupBy fst
        |> List.map (fun (a, b) -> (a, Set.ofList (List.map snd b)))
        |> Map.ofList

    (beforeMap, afterMap)

let part1 (input: string) : string =
    let rules, updates = parseInput input

    let beforeMap, afterMap = createRuleMaps rules
    let hasCorrectOrdering = correctOrdering beforeMap afterMap

    let answer = updates |> List.filter hasCorrectOrdering |> List.sumBy middle


    answer.ToString()

let fixIncorrectUpdate beforeMap update =
    let comp a b =
        let aBeforeB =
            match Map.tryFind a beforeMap with
            | Some after -> Set.contains b after
            | None -> false

        let bBeforeA =
            match Map.tryFind b beforeMap with
            | Some after -> Set.contains a after
            | None -> false

        match aBeforeB, bBeforeA with
        | true, false -> -1
        | false, true -> 1
        | false, false -> 0
        | _ -> failwith "Incorrect ordering"


    update |> List.sortWith comp


let part2 (input: string) : string =
    let rules, updates = parseInput input
    let beforeMap, afterMap = createRuleMaps rules

    let hasIncorrectOrdering = not << correctOrdering beforeMap afterMap

    let answer =
        updates
        |> List.filter hasIncorrectOrdering
        |> List.map (fixIncorrectUpdate beforeMap)
        |> List.sumBy middle

    answer.ToString()
