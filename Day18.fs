module Day18
let day = "18"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(y) = this.FormatRow (this.Row(y))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this

    member this.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.FilterSeq(v) =
        this.Coords ()
        |> Seq.filter (fun (x, y) -> this.[x, y] = v)  // Use Flattern ??

    member this.Find(v) =
        this.Coords ()
        |> Seq.find (fun (x, y) -> this.[x, y] = v)    // new

    member this.TryFind(v) =
        this.Coords ()
        |> Seq.tryFind (fun (x, y) -> this.[x, y] = v) // new

    member this.NHood(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                if this.InBounds (x, y)
                then Some this.[x,y]
                else None |]

    member this.Adjacent(x, y) =
        let nhood = this.NHood (x, y)
        nhood.[4] <- None
        nhood

    member this.Transform(generate: Grid<'a> -> int -> int -> 'a) =
        [| for y in 0 .. maxY do
            [| for x in 0 .. maxX do
                generate this x y |] |]
        |> Grid

    member _.Flattern() = Array.collect id data
    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
    member this.Get(x, y) = this.[x, y]
    member this.Set(x, y) value = this.[x, y] <- value

type Triton = Grid<char>

let tritonGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Triton

let parseLine (line: string) =
    Regex.Match(line, @"(.*)")
    |> fun (m: Match) ->
        let grp (idx: int) = m.Groups.[idx].Value
        let grpi = grp >> int
        grp 1

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let triton () = tritonGrid (readLines day)

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let distanceKeys (triton: Triton) (keys: Set<char>) start =
    // print triton

    let nextPosition (x, y) dir =
        match dir with
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        | East -> (x + 1, y)

    let right = function
        | North -> East | South -> West | West -> North | East -> South
    let left = function
        | North -> West | South -> East | West -> South | East -> North
    let reverse = function
        | North -> South | South -> North | West -> East | East -> West

    let rec distances been pos dist =
        let been = Set.add pos been
        match triton.Get pos with
        | key when keys.Contains key ->  Seq.singleton (Some (key, dist))
        | '#' -> Seq.singleton None
        | door when Char.IsUpper door -> Seq.singleton None
        | _ ->
            [North; South; West; East]
            |> Seq.map (nextPosition pos)
            |> Seq.filter (been.Contains >> not)
            |> Seq.collect (fun next ->
                distances been next (dist + 1))
    distances Set.empty start 0 
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Seq.map (fun (_, keys) -> keys |> Seq.minBy snd)

let nearestKey (triton: Triton) (keys: Set<char>) start =
    distanceKeys triton keys start
    |> Seq.minBy snd

let rec collectKeys (triton: Triton) keys start dist =
    if Set.isEmpty keys then dist else

    let key, keyDist = nearestKey triton keys start
    let keyPos = triton.Find key
    let doorPos = triton.Find (Char.ToUpper key)
    let dist = keyDist + dist
    let keys = keys.Remove key
    triton.Set keyPos '.'
    triton.Set doorPos '.'
    collectKeys triton keys keyPos dist

let Part1 () =
    let triton = triton ()
    let start = triton.Find ('@')
    // distance triton start 'a'
    // findKeys triton start
    let keys = [(int 'a') .. (int 'z')] |> List.map char |> Set
    // nearestKey triton (Set keys) start
    collectKeys triton keys start 0

let Part2 () =
    ()

