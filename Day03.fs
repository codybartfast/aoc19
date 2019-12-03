module Day03
let day = "03"

//#nowarn "0025"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

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


let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let input =
    lines
    |> Array.map (fun line ->
        line.Split(",")
        |> Array.map (fun field -> (field.[0..0], int field.[1..])))


(* ================ Part 1 ================ *)


let instrs1 = input.[0]
let instrs2 = input.[1]

let goUp (x, y) = (x + 1, y)
let goRight (x, y) = (x, y + 1)
let goDown (x, y) = (x - 1, y)
let goLeft (x, y) = (x, y - 1)

let expand (dir, count) =
    let step =
        match dir with
        | "U" -> goUp
        | "R" -> goRight
        | "D" -> goDown
        | "L" -> goLeft
    Seq.init count (fun i -> step)

let apply path inst =
    (expand inst)
    |> Seq.fold
        (fun path f -> (f path.Head)::path)
        path

let wire instrs =
    instrs
    |> Seq.fold apply [0,0]
    |> List.rev
        
let wire1 = wire instrs1
let wire2 = wire instrs2

let coords1 = wire1 |> Set
let coords2 = wire2 |> Set

let intersections = 
    Set.intersect coords1 coords2
    |> Set.remove (0,0)

let Part1 () =

    intersections    
    |> Seq.minBy (fun (x, y) -> (abs x) + (abs y))
    |> snd


(* ================ Part 2 ================ *)


let Part2 () =

    let dist coord =
        List.findIndex ((=) coord) wire1.Tail
        + List.findIndex ((=) coord) wire2.Tail
        + 2

    intersections
    |> Seq.map dist
    |> Seq.min
