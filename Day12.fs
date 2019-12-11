module Day12
let day = "12"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parseLine (line: string) =
    Regex.Match(line, @"(.*)")
    |> fun (m: Match) ->
        let grp (idx: int) = m.Groups.[idx].Value
        let grpi = grp >> int
        grp 1

let input = lines |> Array.map parseLine



let Part1 () =
    input

let Part2 () =
    ()
