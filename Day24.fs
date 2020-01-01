module Day24
let day = "24"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%A" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

type Cells = int * int * int -> int

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())

    member _.LastCol = maxX
    member _.Width = maxX + 1
    member _.LastRow = maxY
    member _.Height = maxY + 1

    static member Generate<'a when 'a : equality>
        (width, height, gen: int -> int -> 'a) =
            [| for y in 0 .. (height - 1) do
                [| for x in 0 .. (width - 1) do
                    gen x y |] |]
            |> Grid<'a>

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member _.Column with get(x) = data |> Array.map (fun arr -> arr.[x])
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

    member this.BorderingCoords(x, y) =
        [| (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y); |]

    member this.Bordering(x, y) =
        this.BorderingCoords(x, y) |> Array.map this.TryGet

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

    member _.Crop(x, width, y, height) =
        data.[y .. (y + height - 1)]
        |> Array.map (fun row -> row.[x .. (x + width - 1)])
        |> Grid<'a>

    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
    member this.Get((x, y)) = this.[x, y]
    member this.Set((x, y)) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

type Eris = Grid<char>

let toEris =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Eris

let toLines line = Regex.Split(line, @"\r?\n")

let test1a = toLines @"....#
#..#.
#..##
..#..
#...."

let update (eris: Eris) x y =
    let bugCount = 
        eris.Bordering (x, y) 
        |> Array.choose id
        |> Array.filter ((=) '#')
        |> Array.length
    match eris.[x, y], bugCount with
    | '#', 1 -> '#'
    | '#', _ -> '.'
    | '.', 1 | '.', 2 -> '#'
    | _ -> '.'

let biodiversity (eris: Eris) =
    eris.Flatern ()
    |> Seq.mapi (fun i (_, cell) -> 
        match cell with
        | '#' -> 1 <<< i
        | _ -> 0 )
    |> Seq.sum

let Part1 () =
    toEris lines
    |> Seq.unfold (fun eris ->
        Some (biodiversity eris, eris.Transform update))
    |> Seq.scan
        (fun (_, seen) bd -> 
            let dup = if Set.contains bd seen then Some bd else None
            (dup, Set.add bd seen))
        (None, Set.empty)
    |> Seq.pick fst

let buildGrids (zeroGrid: Eris) =
    [-201 .. 201]
    |> List.map (fun i -> i, zeroGrid.Transform (fun _ _ _ -> '.') )
    |> Map
    |> Map.add 0 zeroGrid

let getCells (grids: Map<int, Eris>) : Cells = 
    fun (x, y, z) -> 
    match grids.[z].[x, y] with
    | '#' -> 1
    | '.' -> 0

let count (cells: Cells) coords =
    coords
    |> List.sumBy cells
 

let topRow (cells: Cells) z = 
    [(0, 0, z); (1, 0, z); (2, 0, z); (3, 0, z); (4, 0, z)]
    |> count cells

let rightCol cells z = 
    [(4, 0, z); (4, 1, z); (4, 2, z); (4, 3, z); (4, 4, z)]
    |> count cells

let bottomRow cells z = 
    [(0, 4, z); (1, 4, z); (2, 4, z); (3, 4, z); (4, 4, z)]
    |> count cells

let leftCol cells z = 
    [(0, 0, z); (0, 1, z); (0, 2, z); (0, 3, z); (0, 4, z)]
    |> count cells

let bugCount cells (x, y, z) =
    let top = 
        match x, y with
        | _, 0 -> cells (2, 1, (z - 1))
        | 2, 3 -> bottomRow cells (z + 1)
        | _, _ -> cells (x, y - 1, z)
    let right = 
        match x, y with
        | 4, _ -> cells (3, 2, (z - 1))
        | 1, 2 -> leftCol cells (z + 1)
        | _, _ -> cells (x + 1, y, z)
    let bottom = 
        match x, y with
        | _, 4 -> cells (2, 3, (z - 1))
        | 2, 1 -> topRow cells (z + 1)
        | _, _ -> cells (x, y + 1, z)    
    let left = 
        match x, y with
        | 0, _ -> cells (1, 2, (z - 1))
        | 3, 2 -> rightCol cells (z + 1)
        | _, _ -> cells (x - 1, y, z)
    top + right + bottom + left

let update2 bugCount z =
    fun (eris: Eris) x y ->
        match x, y with
        | 2, 2 -> '?'
        | _ ->
            match eris.[x, y], bugCount (x, y, z) with
            | '#', 1 -> '#'
            | '#', _ -> '.'
            | '.', 1 | '.', 2 -> '#'
            | _ -> '.'

let transform grids =
    let range = (Map.count grids / 2)
    let bugCount = bugCount (getCells grids)
    [-range .. range]
    |> List.map (fun z ->
        let newGrid =
            if abs z = range
            then grids.[z]
            else (grids.[z].Transform (update2 bugCount z))
        z, newGrid)
    |> Map

let rec multiTransform grids count =
    match count with
    | 0 -> grids
    | _ -> multiTransform (transform grids) (count - 1)

let bugSum (grids: Map<int, Eris>) =
    grids
    |> Map.toSeq
    |> Seq.collect (fun (_, grid) -> grid.Flatern ())
    |> Seq.filter (snd >> ((=) '#'))
    |> Seq.length

let Part2 () =
    let grids = buildGrids (toEris lines)
    let transformed = (multiTransform grids 200)
    // transformed.Count

    bugSum transformed

    // [-5 .. 5] 
    // |> List.map (fun i -> transformed.[i])
    // |> List.iter (fun grid ->
    //     printfn ""
    //     print grid
    //     printfn "")
    
  
