module Day03
let day = "03"

#nowarn "0025"

open System.IO

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let parseLine (line: string) =
    line.Split(",")
    |> Array.map (fun field -> (field.[0..0], int field.[1..]))

let input = Array.map parseLine lines

let instrs1 = input.[0]
let instrs2 = input.[1]

let expand instr =
    let (direction, count) = instr
    let step = 
        match direction with 
        | "U" -> (fun (x, y) -> (x + 1, y))
        | "R" -> (fun (x, y) -> (x, y + 1))
        | "D" -> (fun (x, y) -> (x - 1, y))
        | "L" -> (fun (x, y) -> (x, y - 1))
    Seq.init count (fun i -> step)

let apply path instr =
    (path, (expand instr))
    ||> Seq.fold (fun pth f -> (f pth.Head)::pth)

let wire instrs = instrs |> Seq.fold apply [0,0] |> List.rev

let wire1 = wire instrs1
let wire2 = wire instrs2

let intersections = Set.intersect (Set wire1.Tail) (Set wire2.Tail)

let dist (x, y) = abs x + abs y

let Part1 () =
    intersections
    |> Seq.minBy dist
    |> snd

let Part2 () =
    let length coord =
        List.findIndex ((=) coord) wire1
        + List.findIndex ((=) coord) wire2

    intersections
    |> Seq.map length
    |> Seq.min
