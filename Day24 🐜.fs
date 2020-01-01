module Day24
let day = "24"

open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

type Cells = int * int * int -> int

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())

    member _.LastCol = maxX
    member _.Width = maxX + 1
    member _.LastRow = maxY
    member _.Height = maxY + 1

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Flatten() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member this.Filter(pred) = this.Flatten() |> Seq.filter (snd >> pred)

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

    member this.Get((x, y)) = this.[x, y]
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

type Eris = Grid<char>

let toEris =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Eris

let lifeOrDeath current adjacent =
    match current, adjacent with
    | '#', 1 -> '#'
    | '#', _ -> '.'
    | '.', 1 | '.', 2 -> '#'
    | _ -> '.'

let update (eris: Eris) x y =
    eris.Bordering (x, y)
    |> Array.choose id
    |> Array.filter ((=) '#')
    |> Array.length
    |> lifeOrDeath eris.[x, y]

let biodiversity (eris: Eris) =
    eris.Flatten ()
    |> Seq.mapi (fun i (_, cell) ->
        match cell with
        | '#' -> 1 <<< i
        | _ -> 0 )
    |> Seq.sum

let buildGrids (zeroGrid: Eris) =
    [-201 .. 201]
    |> List.map (fun i -> i, zeroGrid.Transform (fun _ _ _ -> '.') )
    |> Map
    |> Map.add 0 zeroGrid

let getCells (grids: Map<int, Eris>) : Cells =
    fun (x, y, z) ->
    match grids.[z].[x, y] with
    | '#' -> 1
    | _ -> 0

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

let adjCount cells (x, y, z) =
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

let plutoniaUpdate adjCount z =
    fun (eris: Eris) x y ->
        match x, y with
        | 2, 2 -> '?'
        | _ -> lifeOrDeath eris.[x, y] (adjCount (x, y, z))

let plutonianTransform grids =
    let range = (Map.count grids / 2)
    let bugCount = adjCount (getCells grids)
    [-range .. range]
    |> List.map (fun z ->
        let newGrid =
            if abs z = range
            then grids.[z]
            else (grids.[z].Transform (plutoniaUpdate bugCount z))
        z, newGrid)
    |> Map

let rec multiPlutonianTransform grids count =
    match count with
    | 0 -> grids
    | _ -> multiPlutonianTransform (plutonianTransform grids) (count - 1)

let bugCount (grids: Map<int, Eris>) =
    grids
    |> Map.toSeq
    |> Seq.collect (fun (_, grid) -> grid.Flatten ())
    |> Seq.filter (snd >> ((=) '#'))
    |> Seq.length

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

let Part2 () =
    let grids = buildGrids (toEris lines)
    let transformed = (multiPlutonianTransform grids 200)
    bugCount transformed
