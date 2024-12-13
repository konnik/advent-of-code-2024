module aoc2024_fsharp.Day3

open FParsec

type Mul = int * int

let parseInput1 (input: string) : Mul list =
    // forward declaration of the parser (I miss Haskell)
    let trimGarbageMulP, trimGarbageMulPRef = createParserForwardedToRef<Mul, Unit> ()

    let mulP: Parser<Mul, Unit> =
        tuple2 (skipString "mul(" >>. pint32 .>> skipChar ',') (pint32 .>> skipChar ')')

    trimGarbageMulPRef.Value <- (attempt mulP) <|> (attempt (skipAnyChar >>. trimGarbageMulP))

    match run (many trimGarbageMulP) input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith $"Parse error: {errorMsg}"


let part1 (input: string) : string =
    let multiplications = parseInput1 input
    let multSum = multiplications |> List.sumBy (fun (a, b) -> a * b)
    multSum.ToString()


type Instr =
    | Mul of int * int
    | Do
    | Dont

let parseInput2 (input: string) : Instr list =
    let instrP, instrPRef = createParserForwardedToRef<Instr, Unit> () //I really miss Haskell

    let mulP: Parser<Instr, Unit> =
        tuple2 (skipString "mul(" >>. pint32 .>> skipChar ',') (pint32 .>> skipChar ')')
        |>> Mul

    let doP: Parser<Instr, Unit> = skipString "do()" >>% Do
    let dontP: Parser<Instr, Unit> = skipString "don't()" >>% Dont

    instrPRef.Value <-
        (attempt mulP)
        <|> (attempt doP)
        <|> (attempt dontP)
        <|> (attempt (skipAnyChar >>. instrP))

    match run (many instrP) input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith $"Parse error: {errorMsg}"

let run (instructions: Instr list) : int =
    let step (sum, enabled) instruction =
        match instruction with
        | Mul(a, b) -> if enabled then (sum + a * b, enabled) else (sum, enabled)
        | Do -> (sum, true)
        | Dont -> (sum, false)

    instructions |> List.fold step (0, true) |> fst

let part2 (input: string) : string =
    let instructions = parseInput2 input
    let result = run instructions

    result.ToString()
