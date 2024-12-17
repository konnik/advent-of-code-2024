module aoc2024_fsharp.Util

let lines (input: string) : string list =
    input.Split([| '\n' |], System.StringSplitOptions.None) |> Array.toList

let words (input: string) : string list =
    input.Split([| ' '; '\t'; '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let parseInt (s: string) : int = System.Int32.Parse(s)

let todo (msg: string) = failwith $"TODO: {msg}"

let buildGrid
    (input: string)
    (initialState: 'state)
    (f: (int * int) * char * 'state -> 'a * 'state)
    : Map<int * int, 'a> * 'state =
    let seq: ((int * int) * char) seq =
        lines input
        |> Seq.indexed
        |> Seq.collect (fun (rowIndex, line) ->
            line |> Seq.indexed |> Seq.map (fun (colIndex, c) -> ((rowIndex, colIndex), c)))


    seq
    |> Seq.fold
        (fun (m: Map<int * int, 'a>, s: 'state) (p: int * int, c: char) ->
            let (a, s2) = f (p, c, s)
            (Map.add p a m, s2))
        (Map.empty, initialState)
