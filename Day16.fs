module Day16
let day = "16"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%A" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parse (str: string) =
    str.ToCharArray() |> Array.map (int >> ((+) -48)) 

let test1a = "12345678"
let test1b = "80871224585914546619083218645595"
let test1c = "19617804207202209144916044189917"
let test1d = "69317163492948606335995924319873"

let test2a = "03036732577212944063491565474664"
let test2b = "02935109699940807407585447034323"
let test2c = "03081770884921959731165446850517"

let input = lines.[0]

let seeds = [0; 1; 0; -1]

let pattern col row =
    match ((col + 1) / (row + 1)) % 4 with
    | 0 -> 0 | 1 -> 1 | 2 -> 0  | 3 -> -1 

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

let repeat n array = [|1 .. n|] |> Array.collect (fun i -> array)

let Part1 () =
    let input = (parse input)
    // repeatFn phase 100 signal |> first8
    (phase input).[input.Length / 2 ..]
   
let lastHalf (input: int[]) (output: int[]) =    
    let len = input.Length
    output.[len - 1] <- input.[len - 1]

    seq{len - 2 .. -1 .. len - (len / 2)}
    |> Seq.iter (fun i -> 
        output.[i] <-  output.[i + 1] + input.[i] |> single)


let Part2 () =
    let input = (parse input)    
    // let input = repeat 100 input


    let output= Array.zeroCreate input.Length 
    lastHalf input output
    output.[output.Length / 2 ..]

    // seq{ 10 .. -1 .. 1 }
    