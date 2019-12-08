module Day08
let day = "08"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)


let pixels = 
    lines.[0].ToCharArray () 
    |> Array.map ((Char.GetNumericValue) >> int)


let width = 25
let height = 6
let pixelsPerLayer = width * height
let layerCount = pixels.Length / pixelsPerLayer

let rec splitLayers start = seq {
    if start < pixels.Length then
        yield pixels.[start .. (start + pixelsPerLayer - 1)]
        yield! splitLayers (start + pixelsPerLayer) } 

let layers = splitLayers 0 |> Array.ofSeq


let rec value row col =
    let rec value' layer =
        match layers.[layer].[row * 25 + col] with
        | 2 -> value' (layer + 1)
        | v -> v
    match (value' 0) with
    | 0 -> " "
    | _ -> "X"

let Part1 () =
    let filterLen pred = (Array.filter pred >> Array.length)
    let layer = 
        layers |> Array.minBy (filterLen ((=) 0))
    (filterLen ((=) 1) layer) * (filterLen ((=) 2) layer)

let Part2 () =
    let nl = System.Environment.NewLine
    nl +
        String.concat nl
            [ for row in [0 .. (height - 1)] ->
                String.concat ""
                    [for col in [0 .. (width - 1)] ->
                        value row col ]]

