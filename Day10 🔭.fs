module Day10
let day = "10"

open System
open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let nl = System.Environment.NewLine

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

    member this.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member _.Coords() =
        seq{ for y in 0 .. maxY do for x in 0 .. maxX do yield (x, y) }

    member this.FilterSeq(v) =
        this.Coords () |> Seq.filter (fun (x, y) -> this.[x, y] = v)

    member this.Transform(generate: Grid<'a> -> int -> int -> 'a) =
        [| for y in 0 .. maxY do
            [| for x in 0 .. maxX do
                generate this x y |] |]
        |> Grid

    member this.Get(x, y) = this.[x, y]
    member this.Set(x, y) value = this.[x, y] <- value

type Atlas = Grid<char>

let textGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Atlas

let atlas = textGrid lines

let vect (x, y) (x', y') = (x' - x), (y' - y)

let extend (atlas: Atlas) coord vect =
    let add coord vect = (fst coord) + (fst vect), (snd coord) + (snd vect)
    let rec extend' coord = seq{
        if (atlas.InBounds (coord)) then
            yield coord
            yield! extend' (add coord vect)
        else
            () }
    extend' coord

let markObscured coord =
    let rec hcf a b = if b = 0 then abs a else hcf b (a % b)
    let rec simplify (a, b) =
        match hcf a b with
        | 1 -> (a, b) | f -> simplify (a/f, b/f)

    let atlas = atlas.Copy ()
    atlas.Set coord '█'

    let markObscured' other =
        extend atlas other (simplify (vect coord other))
        |> Seq.tail
        |> Seq.filter (fun coord -> atlas.Get coord = '#')
        |> Seq.iter (fun coord -> atlas.Set coord '·')

    atlas.FilterSeq '#'
    |> Seq.filter ((<>) coord)
    |> Seq.iter markObscured'

    atlas

let visible coord = (markObscured coord).FilterSeq('#')

let angle (a, b) = (atan2 (float -a) (float b) + Math.PI) % (2.0 * Math.PI)

let targets station =
    visible station
    |> Seq.sortBy ((vect station) >> angle)
    |> List.ofSeq

let mutable station = (-1, -1)
let Part1 () =
    let location, count =
        atlas.Coords ()
        |> Seq.map (fun c -> (c, (visible >> Seq.length) c))
        |> Seq.maxBy snd
    station <- location
    count

let Part2 () =
    let (x, y) = targets station |> Seq.item 199
    // let atlas = markObscured station
    // atlas.[x, y] <- 'X'
    // atlas.FormatItem <- function '.' -> " " | '#' -> "•" | c -> c.ToString()
    // printfn "%s%A%s" nl atlas nl
    x * 100 + y
