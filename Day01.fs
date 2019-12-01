module Day01
let day = "01"

(* ================ Part 1 ================ *)


let parseLine = int
let parseInput = Array.map parseLine

let fuel m = max 0 ((m / 3) - 2)

let Part1 (input : string[]) =
    parseInput input
    |> Seq.sumBy fuel


(* ================ Part 2 ================ *)


let rec incFuel m =
    match fuel m with
    | f when f > 0 -> f + incFuel f
    | _ -> 0

let Part2 r1 (input : string[])  =
    parseInput input
    |> Seq.sumBy incFuel
