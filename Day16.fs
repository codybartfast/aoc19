module Day16
let day = "16"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parse (str: string) =
    str.ToCharArray() |> Array.map (int >> ((+) -48)) |> List.ofArray

let test1a = "12345678"
let test1b = "80871224585914546619083218645595"
let test1c = "19617804207202209144916044189917"
let test1d = "69317163492948606335995924319873"

let test2a = "03036732577212944063491565474664"
let test2b = "02935109699940807407585447034323"
let test2c = "03081770884921959731165446850517"

let input = lines.[0]

let seed = [0; 1; 0; -1]
let patterns seed len =
    let rec repeat i = seq { yield i; yield! repeat i}
    let rec repeatSeq sq = seq{ yield! sq; yield! repeatSeq sq}
    let stretch sq n = sq |> Seq.collect (repeat >> Seq.take n)
    [1 .. len]
    |> Seq.map
        ((stretch seed) >> repeatSeq >> Seq.tail >> (Seq.take len) >> List.ofSeq)
    |> List.ofSeq

let apply signal pattern =
    (List.map2 (*) signal pattern)
    |> (List.reduce (+) >> abs >> (fun n -> n % 10))

let phase signal =
    patterns seed (List.length signal)
    |> List.map (apply signal)

let rec repeatFn fn n input =
    match n with
    | 0 -> input
    | _ -> repeatFn fn (n - 1) (fn input)

let first8 signal =
    signal
    |> List.take 8
    |> List.map (fun i -> i.ToString())
    |> (String.concat "")

let repeatList n l = [1 .. n] |> List.collect (fun i -> l)

let Part1 () =
    let signal = parse input
    repeatFn phase 100 signal |> first8

let Part2 () =
    ()