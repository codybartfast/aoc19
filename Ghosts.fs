// GHOSTS OF CHRISTMAS PAST
// ========================

module Ghosts

open System

// strings, chars, hex
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let fromChars (chrs : char[]) = String(chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = (BitConverter.ToString
            >> (fun str ->str.Replace("-", String.Empty)))

let shiftToOrigin coords =
    // aoc18:10
    let xMin = coords |> List.groupBy fst |> List.minBy fst |> fst
    let yMin = coords |> List.groupBy snd |> List.minBy fst |> fst
    coords |> List.map (fun (x,y) -> (x - xMin, y - yMin))

let toRows coords =
    // aoc18:10
    coords |> List.groupBy snd |> List.sortBy fst |> List.map snd

let rec pairCombos = function
    // aoc18:02
    | [] | [_] -> []
    | head::tail ->
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let rec permutations list =
    // aoc15:13
    let rec insertAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)


module Ring =
    // aoc18:09
    type Ring<'a> =
        {   Item : 'a
            mutable Prev : Ring<'a>
            mutable Next : Ring<'a> }

    let singleton item =
        let rec s = {Item = item; Prev = s; Next = s }
        s

    let insert item ring =
        let pair =  {Item = item; Prev = ring; Next = ring.Next }
        ring.Next.Prev <- pair
        ring.Next <- pair
        pair

    let remove ring =
        if ring.Next = ring then failwith "Oops! Last item standing"
        else
            let item = ring.Item
            ring.Prev.Next <- ring.Next
            ring.Next.Prev <- ring.Prev
            ring.Next

    let rec forward n ring =
        match n with
        | 0 -> ring
        | n -> forward (n-1) ring.Next

    let rec back n ring =
        match n with
        | 0 -> ring
        | n -> back (n-1) ring.Prev

type Grid<'a>(jagged: 'a[][]) =
    // aoc15:18
    let nl = System.Environment.NewLine

    let data = jagged
    let maxX = (Array.length data) - 1
    let maxY = (Array.length (data.[0])) - 1

    member _.Item with get(x, y) = data.[x].[y]
    member _.FormatItem = (fun x -> x.ToString())
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(x) = data.[x]
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(x) = this.FormatRow (this.Row(x))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this

    member this.NHood (x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                if x < 0 || x > maxX || y < 0 || y > maxY
                then None
                else Some this.[x,y] |]

    member this.Adjacent (x, y) =
        let nhood = this.NHood (x, y)
        nhood.[4] <- None
        nhood

    member this.Transform (generate: Grid<'a> -> int -> int -> 'a) =
        [| for x in 0 .. maxX do
            [| for y in 0 .. maxY do
                generate this x y |] |]
        |> Grid

    member _.Flattern () =
        Array.collect id data

    member _.Corners () =
        [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]

    member _.Set (x, y) value =
        data.[x].[y] <- value

let textGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Grid<char>
