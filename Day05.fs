module Day05
let day = "05"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let compile (str: string) = str.Split "," |> Array.map int
let mutable program = compile lines.[0]

let mem = Array.copy program
let reset ()  = program |> Array.iteri (fun i v -> mem.[i] <- v)

let mutable sysId = 0
let readInput () = sysId

let mutable lastOutput = -1
let writeOutput value = lastOutput <- value

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

let jumpIfTrue ptr =
    if (arg1 ptr) <> 0
    then (arg2 ptr)
    else ptr + 3

let jumpIfFalse ptr =
    if (arg1 ptr) = 0
    then (arg2 ptr)
    else ptr + 3

let lessThan ptr =
    write
        (read (ptr + 3))
        (if (arg1 ptr) < (arg2 ptr) then 1 else 0)

let equals ptr =
    write
         (read (ptr + 3))
        (if (arg1 ptr) = (arg2 ptr) then 1 else 0)

let operation ptr =
    match opCode ptr with
    | 1 -> add, ptr + 4
    | 2 -> mult, ptr + 4
    | 3 -> input, ptr + 2
    | 4 -> output, ptr + 2
    | 5 -> ignore, jumpIfTrue ptr
    | 6 -> ignore, jumpIfFalse ptr
    | 7 -> lessThan, ptr + 4
    | 8 -> equals, ptr + 4
    | u -> failwithf "Unexpected opCode: %i (ptr: %i)" u ptr

let run id =
    sysId <- id
    let rec execute ptr =
        if isHalt ptr then
            ()
        else
            let (op, next) = operation ptr
            op ptr
            execute next
    execute 0

let Part1 () =
    run 1
    lastOutput



let Part2 () =
    reset ()
    run 5
    lastOutput
    
    // let ex1a = "3,9,8,9,10,9,4,9,99,-1,8"
    // program <- compile ex1a

    // reset ()
    // run 1
    // printfn "with 1: %i" lastOutput
    // reset ()
    // run 8
    // printfn "with 8: %i" lastOutput
    // reset ()
    // run 10
    // printfn "with 10: %i" lastOutput
