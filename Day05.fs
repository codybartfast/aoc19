module Day05
let day = "05"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let program = lines.[0].Split "," |> Array.map int

let mem = Array.copy program
let reset ()  = program |> Array.iteri (fun i v -> mem.[i] <- v)

let readInput () = 1

let writeOutput value = printfn "Output: %i" value

let read addr = mem.[addr]
let write addr value = mem.[addr] <- value
let isHalt addr = read addr = 99

let instr ptr = mem.[ptr]

let posDE n = n % 100
let posC n = n % 1000 / 100
let posB n = n % 10000 / 1000
let posA n = n % 100000 / 10000

let opCode = instr >> posDE

let arg1 ptr =    
    match (instr >> posC) ptr with
    | 0 -> mem.[mem.[ptr + 1]]
    | 1 -> mem.[ptr + 1]
    | u -> failwithf "Unexpected mode for first para: %i (ptr: %i)" u ptr

let arg2 ptr =    
    match (instr >> posB) ptr with
    | 0 -> mem.[mem.[ptr + 2]]
    | 1 -> mem.[ptr + 2]
    | u -> failwithf "Unexpected mode for second para: %i (ptr: %i)" u ptr

let arg3 ptr =    
    match (instr >> posA) ptr with
    | 0 -> mem.[mem.[ptr + 3]]
    | 1 -> mem.[ptr + 3]
    | u -> failwithf "Unexpected mode for thrid para: %i (ptr: %i)" u ptr

let add ptr = write (read (ptr + 3)) (arg1 ptr + arg2 ptr)
let mult ptr = write (read (ptr + 3)) (arg1 ptr * arg2 ptr)
let input ptr = write (read (ptr + 1)) (readInput ())
let output ptr = writeOutput (arg1 ptr)

let operation ptr =
    match opCode ptr with
    | 1 -> add, 4
    | 2 -> mult, 4
    | 3 -> input, 2
    | 4 -> output, 2
    | u -> failwithf "Unexpected opCode: %i (ptr: %i)" u ptr

let rec run ptr =
    if isHalt ptr then 
        ()        
    else
        let (op, len) = operation ptr
        op ptr
        run (ptr + len)

let Part1 () =
    run 0


let Part2 () =
    ()
