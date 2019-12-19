module Day18
let day = "18"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let toLines str = Regex.Split(str, @"\r?\n")

let test1d = toLines @"########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"

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

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member this.Flatern() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.Filter(pred) = this.Flatern() |> Seq.filter (snd >> pred)

    member this.Find(pred) = this.Filter(pred) |> Seq.head |> fst

    member this.NHood(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                if this.InBounds (x, y)
                then Some this.[x,y]
                else None |]

    member this.Adjacent(x, y) =
        let nhood = this.NHood (x, y)
        Array.append nhood.[0 .. 3] nhood.[5 .. 8]

    member this.Bordering(x, y) =
        [| this.TryGet (x, y - 1);
           this.TryGet (x + 1, y);
           this.TryGet (x, y + 1);
           this.TryGet (x - 1, y); |]

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
    member this.Get((x, y)) = this.[x, y]
    member this.Set((x, y)) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

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
let input = readLines day
let triton lines = tritonGrid (lines)

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let explore (triton: Triton) start isTarget contOnFind =
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

    let rec explore been doors pos dist =
        let been = Set.add pos been
        [North; South; West; East]
        |> Seq.map (nextPosition pos)
        |> Seq.filter (been.Contains >> not)
        |> Seq.collect (fun next -> seq{
            match triton.Get next with
            | '#' -> ()
            | key when (isTarget key) ->
                yield (Some ((key, List.rev doors), dist))
                if contOnFind then yield! explore been doors next (dist + 1)
            | door when Char.IsUpper door ->
                yield! explore been (door::doors) next (dist + 1) 
            | _ -> yield! explore been doors next (dist + 1) })

    explore Set.empty [] start 0
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Seq.map (fun (_, keys) -> keys |> Seq.minBy snd)
    |> List.ofSeq

let directDistances triton  start = 
    explore triton start (Char.IsLower) true

let Part1 () =
    let triton = triton input
    let start = triton.Find ((=) '@')
    directDistances triton start
    |> List.iter (printfn "%A")




let Part2 () =
    ()

