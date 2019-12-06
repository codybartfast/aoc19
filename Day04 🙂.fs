module Day04
let day = "04"

open System.IO

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let (low, high) =
    let parts = lines.[0].Split("-")
    (int parts.[0], int parts.[1])
let pwdLen = 6

let candidates low high pwdLen = seq{
    for n in low .. high do
        n.ToString().PadLeft(pwdLen, '0').ToCharArray()
        |> Array.map (int >> (fun n -> n - 48))
        |> List.ofArray }

let rec isIncreasing = function
    | a::b::t -> a <= b && isIncreasing (b::t)
    | _ -> true

let rec hasDouble = function
    | a::b::t -> if a = b then true else hasDouble (b::t)
    | _ -> false

let rec hasExactDouble = function
    | a::b::c::t when a = b -> 
        if b <> c 
        then true 
        else hasExactDouble (List.skipWhile ((=) b) t)
    | a::[b] when a = b -> true
    | _::t -> hasExactDouble t
    | _ -> false

let Part1 () =
    candidates low high pwdLen
    |> Seq.filter isIncreasing
    |> Seq.filter hasDouble
    |> Seq.length

let Part2 () =
    candidates low high pwdLen
    |> Seq.filter isIncreasing
    |> Seq.filter hasExactDouble
    |> Seq.length
