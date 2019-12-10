module Day10
let day = "10"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let nl = System.Environment.NewLine

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    member _.Item with get(x, y) = data.[y].[x]
    member _.FormatItem = (fun x -> x.ToString())
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(y) = this.FormatRow (this.Row(y))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this // format()

    member this.InBounds (x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy () = this.Transform (fun g x y -> g.[x,y])

    member _.Coords () =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do   
                     yield (x, y) }

    member this.FilterSeq (v) =
        this.Coords ()
        |> Seq.filter (fun (x, y) -> this.[x, y] = v)

    member this.NHood (x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                if x < 0 || x > maxX || y < 0 || y > maxY // contains?
                then None
                else Some this.[x,y] |]

    member this.Adjacent (x, y) =
        let nhood = this.NHood (x, y)
        nhood.[4] <- None
        nhood

    member this.Transform (generate: Grid<'a> -> int -> int -> 'a) =
        [| for y in 0 .. maxY do
            [| for x in 0 .. maxX do
                generate this x y |] |]
        |> Grid

    member _.Flattern () = //format
        Array.collect id data

    member _.Corners () =
        [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]

    member _.Set (x, y) value =
        data.[y].[x] <- value

let textGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Grid<char>

let test1a = @"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"

let test1d = @".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"

let grid = textGrid lines
// let grid = textGrid (test1d.Split(nl))


let rec hcf a b = if b = 0 then abs a else hcf b (a % b)
let rec simplify (a, b) =
    match hcf a b with
    | 1 -> (a, b) | f -> simplify (a/f, b/f)
let vect (x, y) (x', y') = (x' - x), (y' - y)
let add coord vect = (fst coord) + (fst vect), (snd coord) + (snd vect)

let extend (grid: Grid<char>) coord vect = 
    let rec extend' coord = seq{
        if not (grid.InBounds (coord)) then 
            ()
        else 
            yield coord
            yield! extend' (add coord vect) }
    List.ofSeq (extend' coord)

let markObscured coord =    
    let grid = grid.Copy ()
    grid.Set coord '█'
    let markObscured' other =
        extend grid other (simplify (vect coord other))
        |> Seq.tail
        |> Seq.iter (fun coord -> grid.Set coord 'o')
    grid.FilterSeq '#'
    |> Seq.filter ((<>) coord)
    |> Seq.iter markObscured'
    grid

let visible coord = (markObscured coord).FilterSeq('#')
let visCount (grid: Grid<char>) = grid.FilterSeq('#') |> Seq.length

let mutable station = (-1, -1)

let Part1 () =
    let location, seen =
        grid.Coords()
        |> Seq.map (fun c -> (c, (visible >> Seq.length) c))
        |> Seq.maxBy snd
    station <- location
    seen    

let angle (a, b) = (atan2 (float -a) (float b) + Math.PI) % (2. * Math.PI)

let targets () =
    visible station
    |> Seq.sortBy ((vect station) >> angle)
    |> List.ofSeq


let Part2 () = 
    targets () 
    |> Seq.item 199
    |> fun (x, y) -> x * 100 + y

