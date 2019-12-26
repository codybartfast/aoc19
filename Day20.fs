module Day20
let day = "20"

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

    member _.LastCol = maxX
    member _.Width = maxX + 1
    member _.LastRow = maxY
    member _.Height = maxY + 1

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member _.Col with get(x) = data |> Array.map (fun arr -> arr.[x])
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

    member this.Crop(x, width, y, height) =
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
    
type Pluto = Grid<string>

let plutoGrid =
    Array.map (fun (s: string) ->
        s.ToCharArray() |> Array.map string)
    >> Pluto

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let input = readLines day

let isText str = Regex.IsMatch(str, @"[A-Z]")

let labeledPluto input =
    let pluto = plutoGrid input
    let labels = 
        pluto.Filter isText
        |> Seq.map (fun ((x,y), letter) -> 
            (x, y), letter, 
            pluto.TryGet (x + 1, y), 
            pluto.TryGet (x, y + 1))
        |> Seq.filter (fun (_, _, right, bottom) ->
            match right, bottom with
            | Some right, _ when isText right -> true
            | _, Some bottom when isText bottom -> true
            | _ -> false)
        |> Seq.map (fun ((x, y), ltr1, right, bottom) ->
            match right, bottom with
            | Some right, _ when isText right -> 
                (x, y), ltr1, (x + 1, y), right
            | _, Some bottom ->
                (x, y), ltr1, (x, y + 1), bottom)

    labels
    |> Seq.iter (fun (loc1, ltr1, loc2, ltr2) ->
        let bordering = 
            Set.union 
                (Set (pluto.BorderingCoords loc1))
                (Set (pluto.BorderingCoords loc2))
        let target = 
            bordering
            |> Seq.filter (fun coord ->
                match pluto.TryGet coord with
                | None | Some " " -> false
                | Some str when isText str -> false
                | Some "." -> true
                | _ -> failwith "Think boy think" )
            |> Seq.exactlyOne
        pluto.Set target (ltr1 + ltr2)
        pluto.Set loc1 " "
        pluto.Set loc2 " ")
           
    // let pluto = pluto.Crop(1, pluto.Width - 2, 1, pluto.Height - 2)
    let pluto = pluto.Transform (fun pluto x y ->
        match pluto.[x, y] with
        | " " -> "#" | str -> str)

    pluto.FormatItem <- function
        | " " -> "  "
        | "." -> "<>"
        | "#" -> "██"
        | str -> str
    pluto

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let explore (pluto: Pluto) ((coord, portal), dist) =
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

    let rec explore been hist pos dist =
        let been = Set.add pos been
        [North; South; West; East]
        |> Seq.map (nextPosition pos)
        |> Seq.filter (been.Contains >> not)
        |> Seq.collect (fun nextPos -> 
            match pluto.Get nextPos with
            | "#" -> Seq.empty
            | "." -> explore been hist nextPos (dist + 1) 
            | portal -> Seq.singleton(Some ((nextPos, portal), dist)) )

    explore Set.empty [] coord 1
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Seq.map ((fun (_, portals) -> portals |> Seq.minBy snd)
        >> (fun ((coord, portal), d) -> ((coord, portal), dist + d)))
    |> List.ofSeq

let usePortal (pluto: Pluto) =
    let portals =
        pluto.Filter isText
        |> List.ofSeq
        |> List.groupBy snd
        |> Map
    fun (coord, label) ->
        portals.[label]
        |> List.filter (fun (c, _) -> c <> coord)
        |> List.exactlyOne

let distances (pluto: Pluto) = 
    let usePortal = usePortal pluto

    let rec distances (dists: Map<string,int>) (starts: list<((int * int) * string) * int>) =
        if starts = [] then dists else

        let shorter =
            starts
            |> List.collect (explore pluto)
            |> List.filter (fun ((_, portal), dist) ->
                not (Map.containsKey portal dists) ||
                    dist < dists.[portal])

        let dists = 
            (dists, shorter)
            ||> List.fold 
                (fun dists ((_, portal), dist) ->
                    (Map.add portal dist dists))
 
        shorter
        |> List.filter (fun ((_, portal), dist) ->
            portal <> "AA" && portal <> "ZZ")
        |> List.map (fun ((coord, portal), dist) ->
            (usePortal (coord, portal)), dist + 1)
        |> (distances dists)
    
    let start = pluto.Find ((=) "AA")
    distances Map.empty [((start, "AA"), 0)]



let Part1 () =
    let pluto = labeledPluto input
    // pluto
    // let iF = pluto.Filter ((=) "IF") |> Seq.head
    // print iF
    // usePortal pluto iF

    // let start = pluto.Find ((=) "AA")
    // explore pluto ((start, "AA"), 0)
    
    (distances pluto).["ZZ"]

let Part2 () =
    ()
