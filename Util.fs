module aoc2024_fsharp.Util

let lines (input: string) : string list =
    input.Split([| '\n' |], System.StringSplitOptions.None) |> Array.toList

let words (input: string) : string list =
    input.Split([| ' '; '\t'; '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let parseInt (s: string) : int = System.Int32.Parse(s)

let todo (msg: string) = failwith $"TODO: {msg}"
