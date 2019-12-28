module Day22
let day = "22"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%A" obj)
let tPrint obj = (print obj); obj

let toLines (text: string) = Regex.Split(text.Trim(), @"\r?\n")

let test1a = toLines @"deal with increment 7
deal into new stack
deal into new stack"

let test1b = toLines @"cut 6
deal with increment 7
deal into new stack"

let test1c = toLines @"deal with increment 7
deal with increment 9
cut -2"

let test1d = toLines @"
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
"

type Deal =
    | Increment of int
    | Cut of int
    | Stack

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parseLine (line: string) =
    if line.Contains("stack") then
        Stack
    else
        let num = Int32.Parse(Regex.Match(line, @"-?\d+").Value)
        if line.StartsWith("deal")
        then Increment num
        else Cut num

let technique instruction =
    match instruction with
    | Stack -> Array.rev
    | Cut n -> (fun arr ->
        let n = if n >= 0 then n else (arr.Length + n )
        Array.append arr.[n ..] arr.[0 .. (n - 1)])
    | Increment n -> (fun arr ->
        let len = arr.Length
        let arr' = Array.zeroCreate len
        [0 .. (len - 1)]
        |> List.iter (fun i ->
            arr'.[(i * n) % len] <- arr.[i])
        arr' )

let shuffle deck instructions =
    (deck, instructions)
    ||> Array.fold (fun deck instr ->
        ((technique instr) deck))

let Part1 () =
    let instructions = lines |> Array.map parseLine
    let deck = [|0 .. 10_006|]

    (shuffle deck instructions)
    |> Array.findIndex ((=) 2019)

let Part2 () = ()