module aoc2024_fsharp.Day4

open aoc2024_fsharp.Util

type Grid = Map<int * int, char>

let parseInput (input: string) : Grid =

    let charMap =
        lines input
        |> Seq.indexed
        |> Seq.collect (fun (rowIndex, line) ->
            line |> Seq.indexed |> Seq.map (fun (colIndex, c) -> ((rowIndex, colIndex), c)))
        |> Map.ofSeq

    charMap

type Four<'a> = Four of 'a * 'a * 'a * 'a

type Pos = int * int

let XMAS = Four('X', 'M', 'A', 'S')

let offsets =
    [ Four((0, 0), (0, 1), (0, 2), (0, 3))
      Four((0, 0), (1, 0), (2, 0), (3, 0))
      Four((0, 0), (1, 1), (2, 2), (3, 3))
      Four((0, 0), (0, -1), (0, -2), (0, -3))
      Four((0, 0), (-1, 0), (-2, 0), (-3, 0))
      Four((0, 0), (-1, -1), (-2, -2), (-3, -3))
      Four((0, 0), (1, -1), (2, -2), (3, -3))
      Four((0, 0), (-1, 1), (-2, 2), (-3, 3)) ]

let offset (a: Pos) (b: Pos) = (fst a + fst b, snd a + snd b)

let mapFour f (Four(a, b, c, d)) = Four(f a, f b, f c, f d)
let offsetFour pos four = four |> mapFour (offset pos)

let wordsAt (grid: Grid) (pos: Pos) : Four<char> seq =
    let charAt pos =
        Map.tryFind pos grid |> Option.defaultValue '.'

    offsets |> Seq.map (offsetFour pos) |> Seq.map (mapFour charAt)



let part1 (input: string) : string =
    let grid = parseInput input

    let xmasCount =
        grid
        |> Map.keys
        |> Seq.collect (wordsAt grid)
        |> Seq.filter ((=) XMAS)
        |> Seq.length

    xmasCount.ToString()


type Five<'a> = Five of 'a * 'a * 'a * 'a * 'a

let XMAS2 = Five('M', 'A', 'S', 'M', 'S')

let offsets2 =
    [ Five((-1, -1), (0, 0), (1, 1), (1, -1), (-1, 1))
      Five((-1, -1), (0, 0), (1, 1), (-1, 1), (1, -1))
      Five((1, 1), (0, 0), (-1, -1), (1, -1), (-1, 1))
      Five((1, 1), (0, 0), (-1, -1), (-1, 1), (1, -1)) ]


let mapFive f (Five(a, b, c, d, e)) = Five(f a, f b, f c, f d, f e)
let offsetFive pos five = five |> mapFive (offset pos)

let crossAt (grid: Grid) (pos: Pos) : Five<char> seq =
    let charAt pos =
        Map.tryFind pos grid |> Option.defaultValue '.'

    offsets2 |> Seq.map (offsetFive pos) |> Seq.map (mapFive charAt)


let part2 (input: string) : string =
    let grid = parseInput input

    let xmasCount =
        grid
        |> Map.keys
        |> Seq.collect (crossAt grid)
        |> Seq.filter ((=) XMAS2)
        |> Seq.length

    xmasCount.ToString()
