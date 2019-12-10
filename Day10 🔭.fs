module Day10
let day = "10"

open System
open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let roids =
    lines
    |> Array.mapi (fun y line ->
        line.ToCharArray ()
        |> Array.mapi (fun x c -> 
            match c with
            | '#' -> Some (x, y) 
            | _ -> None)
        |> Array.choose id)
    |> Array.collect id

let vect (x, y) (x', y') = (x' - x), (y' - y)
let angle (a, b) = (atan2 (float -a) (float b) + Math.PI) % (2.0 * Math.PI)

let visiblFrom coord = roids |> Array.groupBy ((vect coord) >> angle)

let mutable visible = Array.empty

let Part1 () =
    let (location, alligned) = 
        roids
        |> Array.map (fun coord -> coord, (visiblFrom coord))
        |> Array.sortByDescending (snd >> Array.length)
        |> Array.head
    visible <- alligned
    alligned.Length
    
let Part2 () =
    visible
    |> Array.sortBy fst
    |> Array.item 199
    |> snd
    |> Array.minBy (fun (x, y) -> (x * x) + (y * y))
    |> (fun (x, y) -> x * 100 + y)