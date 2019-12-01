module Day00

//#nowarn "0025"

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let nl = System.Environment.NewLine

type Grid<'a>(jagged: 'a[][]) =
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
                generate this x y
            |]
        |] |> Grid

    member _.Flattern () =
        Array.collect id data

    member _.Corners () =
        [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]

    member _.Set (x, y) value =
        data.[x].[y] <- value

let textGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Grid<char>

let display obj = (printfn "%O" obj); obj
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let fromChars (chrs : char[]) = String(chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = (BitConverter.ToString
            >> (fun str ->str.Replace("-", String.Empty)))
let groupValue (m:Match) (idx:int) = m.Groups.[idx].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let lnSplit str = rxSplit @"\r?\n" str
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length


(* ================ Part 1 ================ *)

let parseLine = 
    rxMatch "(\w+) (\d)"
    >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grp 1, grpi 2

let Part1 (input : string[]) =
    input
    //|> Array.map parseLine


(* ================ Part 2 ================ *)


let Part2 result1 (input : string[]) =
    "result2"
