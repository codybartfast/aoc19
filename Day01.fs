module Day01


(* ================ Part 1 ================ *)


let parseInput input =
    input |> Array.map int

let fuel m =
    max 0 (m / 3) - 2

let Part1 (input : string[]) =
    input
    |> parseInput
    |> Array.sumBy fuel


(* ================ Part 2 ================ *)


let rec incFuel m =
    match fuel m with
    | z when z <= 0 -> 0
    | f -> f + incFuel f

let Part2 result1 (input : string[]) =
    input
    |> parseInput
    |> Seq.sumBy incFuel
