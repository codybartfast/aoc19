module Day04
let day = "04"

//#nowarn "0025"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let parseLine (line: string) =
    Regex.Match(line, @"(.*)")
    |> fun (m: Match) ->
        let grp (idx: int) = m.Groups.[idx].Value
        let grpi = grp >> int
        grp 1

let (low, high) =
    let [|low; high|] = lines.[0].Split("-") |> Array.map int
    (low, high)
let pwdLen = 6

let candidates low high pwdLen = 
    [ for n in low .. high do
        let digits = 
            n.ToString().PadLeft(pwdLen, '0').ToCharArray()
            |> Array.map (int >> (fun n -> n - 48))
            |> List.ofArray
        digits ]

let rec hasDouble candidate =
    match candidate with
    | [_] | [] -> false
    | a::(b::_) when a = b -> true
    | _::t -> hasDouble t

let rec isIncreasing candidate =
    match candidate with
    | [_] | [] -> true
    | a::(b::t) -> a <= b && isIncreasing (b::t)


let Part1 () =
    candidates low high pwdLen
    |> List.filter hasDouble
    |> List.filter isIncreasing
    |> List.length


let Part2 () =
    ()
