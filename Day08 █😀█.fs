module Day08
let day = "08"

open System
open System.IO

let nl = System.Environment.NewLine
let filterCount pred = (Array.filter pred >> Array.length)

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let pixels =
    lines.[0].ToCharArray () |> Array.map (Char.GetNumericValue >> int)

let width = 25
let height = 6
let area = width * height
let depth = pixels.Length / area

let layers =
    [|for z in [0 .. depth - 1] ->
        pixels.[z * area .. ((z + 1) * area) - 1]|]

let render () =
    let rec renderPixel row col layer =
        match layers.[layer].[row * 25 + col] with
        | 2 -> renderPixel row col (layer + 1)
        | 1 -> "█"
        | _ -> " "

    String.concat nl
        [ for row in [0 .. (height - 1)] ->
            String.concat ""
                [for col in [0 .. (width - 1)] ->
                    renderPixel row col 0]]

let Part1 () =
    let layer = layers |> Array.minBy (filterCount ((=) 0))
    (filterCount ((=) 1) layer) * (filterCount ((=) 2) layer)

let Part2 () = nl + render () + nl
