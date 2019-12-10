module Day10
let day = "10"

open System
open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let asteroids =
    lines
    |> Array.mapi (fun y line ->
        line.ToCharArray ()
        |> Array.mapi (fun x c ->
            match c with
            | '#' -> Some (x, y)
            | _ -> None)
        |> Array.choose id)
    |> Array.collect id

let visiblFrom observer =
    let vect (x, y) (x', y') = (x' - x), (y' - y)
    let angle (a, b) =
        (atan2 (float -a) (float b) + Math.PI) % (2.0 * Math.PI)
    asteroids |> Array.groupBy ((vect observer) >> angle)

let mutable visibleFromStation = Array.empty

let Part1 () =
    visibleFromStation <-
        asteroids
        |> Array.map visiblFrom
        |> Array.sortByDescending Array.length
        |> Array.head
    visibleFromStation.Length

let Part2 () =
    visibleFromStation
    |> Array.sortBy fst
    |> Array.item 199
    |> snd
    |> Array.minBy (fun (x, y) -> (x * x) + (y * y))
    |> (fun (x, y) -> x * 100 + y)