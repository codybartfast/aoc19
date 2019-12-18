module Day16
let day = "16"

open System
open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parse (str: string) = str.ToCharArray() |> Array.map (int >> ((+) -48))

let input = lines.[0]

let seeds = [0; 1; 0; -1]

let pattern col row =
    match ((col + 1) / (row + 1)) % 4 with
    | 0 -> 0 | 1 -> 1 | 2 -> 0  | 3 -> -1 | _ -> failwith "shh"

let value (signal: int[]) row col = signal.[col] * pattern col row

let single = abs >> (fun n -> n % 10)

let rowValue (signal: int[]) row =
    Array.init (signal.Length) id
    |> Array.sumBy (value signal row)
    |> single

let phase (signal: int[]) =
    Array.init (signal.Length) id
    |> Array.map (fun row -> rowValue signal row)

let rec repeatFn fn n input =
    match n with
    | 0 -> input
    | _ -> repeatFn fn (n - 1) (fn input)

let first8 output =
    output
    |> Seq.take 8
    |> Seq.map (fun i -> i.ToString())
    |> (String.concat "")

let repeatArr n array = [|1 .. n|] |> Array.collect (fun i -> array)

let lastPart start (input: int[]) =
    let len = input.Length
    assert (start >= (len / 2))

    seq{len - 2 .. -1 .. start}
    |> Seq.iter (fun i ->
        input.[i] <-  input.[i + 1] + input.[i] |> single)
    input

let offset =
    Array.take 7 >> Array.map string >> String.concat "" >> Int32.Parse

let Part1 () =
    let input = (parse input)
    (repeatFn phase 100 input) |> first8

let Part2 () =
    let input = (parse input)
    let offset = offset input

    let mega = repeatArr 10_000 input
    repeatFn (lastPart offset) 100 mega |> ignore
    mega.[offset .. offset + 7]
    |> Array.map string
    |> String.concat ""
