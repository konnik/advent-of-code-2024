module aoc2024_fsharp.Day6

open aoc2024_fsharp.Util

// Types

type Pos = int * int

type Direction =
    | Up
    | Left
    | Down
    | Right

type Cell =
    | Empty
    | Obstruction

type Grid = Map<Pos, Cell>

// Utils

let turnClockwise (dir: Direction) : Direction =
    match dir with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let step (dir: Direction) (pos: Pos) : Pos =
    let row, col = pos

    match dir with
    | Up -> (row - 1, col)
    | Right -> (row, col + 1)
    | Down -> (row + 1, col)
    | Left -> (row, col - 1)

let initGrid (input: string) : Grid * Pos =
    buildGrid input (0, 0) (fun (pos, ch, startPos) ->
        match ch with
        | '^' -> (Empty, pos)
        | '#' -> (Obstruction, startPos)
        | _ -> (Empty, startPos))


[<TailCall>]
let rec predictGuardPositions (grid: Grid) (pos: Pos) (dir: Direction) (visited: Set<Pos>) : Set<Pos> =
    let next = pos |> step dir

    match Map.tryFind next grid with
    | Some Obstruction -> predictGuardPositions grid pos (turnClockwise dir) visited
    | Some Empty -> predictGuardPositions grid next dir (visited |> Set.add pos)
    | None -> visited |> Set.add pos


/// Run the simulation until the guard leaves the mapped area or get stuck in a loop.
/// Returns true if looping, otherwise false.
[<TailCall>]
let rec checkForLoops (grid: Grid) (pos: Pos) (dir: Direction) (visited: Set<Pos * Direction>) : bool =
    if Set.contains (pos, dir) visited then
        true
    else
        let next = step dir pos

        match Map.tryFind next grid with
        | Some Obstruction -> checkForLoops grid pos (turnClockwise dir) visited
        | Some Empty -> checkForLoops grid next dir (visited |> Set.add (pos, dir))
        | None -> false

/// Check if a candidate position will create a loop by modifying the original grid
/// and running the simulation on the modified grid.
let createsLoop (grid: Grid) (startPos: Pos) (candidate: Pos) : bool =
    let modifiedGrid = Map.add candidate Obstruction grid
    checkForLoops modifiedGrid startPos Up Set.empty

/// Solution for part 1
let part1 (input: string) : string =
    let grid, startPos = initGrid input
    let answer = predictGuardPositions grid startPos Up Set.empty |> Set.count
    answer.ToString()

/// Solution for part 2
let part2 (input: string) : string =
    let grid, startPos = initGrid input


    let candidates =
        predictGuardPositions grid startPos Up Set.empty |> Set.remove startPos

    let answer = candidates |> Set.filter (createsLoop grid startPos) |> Set.count

    answer.ToString()
