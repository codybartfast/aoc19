module Day01
open System.IO

let day = "01"

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let parseLine = int
let parseInput = Array.map parseLine
let input =
    let parseLine = int
    lines |> Array.map parseLine


(* ================ Part 1 ================ *)


let fuel m = max 0 ((m / 3) - 2)

let Part1 () =
    input |> Seq.sumBy fuel


(* ================ Part 2 ================ *)


let rec incFuel m =
    match fuel m with
    | f when f > 0 -> f + incFuel f
    | _ -> 0

let Part2 ()  =
    input |> Seq.sumBy incFuel
