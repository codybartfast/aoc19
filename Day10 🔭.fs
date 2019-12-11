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

// azimuth = angle from north
let asteroidsByAzimuthFrom location =
    let vect (x, y) (x', y') = (x' - x), (y' - y)
    let azimuth (a, b) =
        (atan2 (float -a) (float b) + Math.PI) % (2.0 * Math.PI)
    asteroids
    |> Array.filter ((<>) location)
    |> Array.groupBy ((vect location) >> azimuth)

let visibleFromStation =
    asteroids
    |> Array.map asteroidsByAzimuthFrom
    |> Array.sortByDescending Array.length
    |> Array.head

let Part1 () = visibleFromStation |> Array.length

let Part2 () =
    visibleFromStation
    |> Array.sortBy fst
    |> Array.item 199
    |> snd
    |> Array.minBy (fun (x, y) -> (x * x) + (y * y))
    |> (fun (x, y) -> x * 100 + y)
