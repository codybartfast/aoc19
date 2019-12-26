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

    member this.Flatern() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member this.Filter(pred) = this.Flatern() |> Seq.filter (snd >> pred)

    member this.Find(pred) = this.Filter(pred) |> Seq.head |> fst

    member this.BorderingCoords(x, y) =
        [| (x, y - 1); (x + 1, y); (x, y + 1); (x - 1, y); |]

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

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

    let pluto = pluto.Transform (fun pluto x y ->
        match pluto.[x, y] with
        | " " -> "#" | str -> str)

    pluto.FormatItem <- function
        | " " -> "  "
        | "." -> "<>"
        | "#" -> "██"
        | str -> str
    pluto

let toLines str = Regex.Split(str, @"\r?\n")

let test1a = toLines @"         A
         A
  #######.#########
  #######.........#
  #######.#######.#
  #######.#######.#
  #######.#######.#
  #####  B    ###.#
BC...##  C    ###.#
  ##.##       ###.#
  ##...DE  F  ###.#
  #####    G  ###.#
  #########.#####.#
DE..#######...###.#
  #.#########.###.#
FG..#########.....#
  ###########.#####
             Z
             Z       "

let test2a = toLines @"             Z L X W       C
             Z P Q B       K
  ###########.#.#.#.#######.###############
  #...#.......#.#.......#.#.......#.#.#...#
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
  #.#...#.#.#...#.#.#...#...#...#.#.......#
  #.###.#######.###.###.#.###.###.#.#######
  #...#.......#.#...#...#.............#...#
  #.#########.#######.#.#######.#######.###
  #...#.#    F       R I       Z    #.#.#.#
  #.###.#    D       E C       H    #.#.#.#
  #.#...#                           #...#.#
  #.###.#                           #.###.#
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#
CJ......#                           #.....#
  #######                           #######
  #.#....CK                         #......IC
  #.###.#                           #.###.#
  #.....#                           #...#.#
  ###.###                           #.#.#.#
XF....#.#                         RF..#.#.#
  #####.#                           #######
  #......CJ                       NM..#...#
  ###.#.#                           #.###.#
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#
  #.....#        F   Q       P      #.#.#.#
  ###.###########.###.#######.#########.###
  #.....#...#.....#.......#...#.....#.#...#
  #####.#.###.#######.#######.###.###.#.#.#
  #.......#.......#.#.#.#.#...#...#...#.#.#
  #####.###.#####.#.#.#.#.###.###.#.###.###
  #.......#.....#.#...#...............#...#
  #############.#.#.###.###################
               A O F   N
               A A D   M                     "

let to2D (x, y, z) = (x, y), z
let to3D (x, y) z = (x, y, z)
let depth (_, _, z) = z
let isRecursive portal = portal <> "AA" && portal <> "ZZ"
let isOuter (pluto: Pluto) (x, y, _)=
    x = 2 || x = pluto.LastCol - 2 ||
        y = 2 || y = pluto.LastRow - 2

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let explore (pluto: Pluto) part1 ((coord, _portal), dist)  =
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

    let levelPortal coord portal =
        let x, y, depth = coord
        match depth, portal with
        | 0, "AA" -> Some (coord, portal)
        | 0, "ZZ" -> Some (coord, portal)
        | 0, _ when (isOuter pluto coord) -> None
        | _, "AA" -> None
        | _, "ZZ" -> None
        | _  -> Some (coord, portal)

    let rec explore been hist coord dist =
        let coord, depth = to2D coord
        let been = Set.add coord been
        [North; South; West; East]
        |> Seq.map (nextPosition coord)
        |> Seq.filter (been.Contains >> not)
        |> Seq.collect (fun nextCoord ->
            let nextCoord3D = to3D nextCoord depth
            match pluto.Get nextCoord with
            | "#" -> Seq.empty
            | "." -> explore been hist nextCoord3D (dist + 1)
            | portal ->
                match part1 with
                | true -> Seq.singleton(Some ((nextCoord3D, portal), dist))
                | false ->
                    match levelPortal nextCoord3D portal with
                    | None -> Seq.empty
                    | Some (nextCoord3D, portal) ->
                         Seq.singleton(Some ((nextCoord3D, portal), dist)) )

    explore Set.empty [] coord 1
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Seq.map ((fun (_, portals) -> portals |> Seq.minBy snd)
        >> (fun ((coord, portal), d) -> ((coord, portal), dist + d)))
    |> List.ofSeq

let distances (pluto: Pluto) part1 finish  =
    let usePortal =
        let portals =
            pluto.Filter isText
            |> List.ofSeq
            |> List.groupBy snd
            |> Map
        fun ((x, y, depth), label) ->
            portals.[label]
            |> List.filter (fun (c, _) -> c <> (x, y))
            |> List.exactlyOne
            |> fun ((x, y), portal) ->
                let depthDiff = if part1 then 0 else 1
                if isOuter pluto (x, y, depth) then
                    (x, y, depth + depthDiff), portal
                else
                    (x, y, depth - depthDiff), portal

    let rec distances dists starts =
        if Map.containsKey finish dists then dists.[finish] else

        let shorter =
            starts
            |> List.collect (explore pluto part1)
            |> List.filter (fun ((coord, portal), dist) ->
                not (Map.containsKey (coord, portal) dists) ||
                    dist < dists.[coord, portal])
            |> List.sortDescending

        let dists =
            (dists, shorter)
            ||> List.fold
                (fun dists ((coord, portal), dist) ->
                    (Map.add (coord, portal) dist dists))

        shorter
        |> List.filter (fun (((_, _, z), portal), dist) ->
            portal <> "AA" && portal <> "ZZ")
        |> List.map (fun ((coord, portal), dist) ->
            (usePortal (coord, portal)), dist + 1)
        |> (distances dists)

    let startLoc = to3D (pluto.Find ((=) "AA")) 0
    let startDist = ((startLoc, "AA"), 0)
    distances
        (Map [startDist])
        [startDist]

let Part1 () =
    let pluto = labeledPluto input
    let finish = (to3D (pluto.Find ((=) "ZZ")) 0, "ZZ")
    distances pluto true finish

let Part2 () =
    let pluto = labeledPluto input
    let finish = (to3D (pluto.Find ((=) "ZZ")) 0, "ZZ")
    distances pluto false finish
