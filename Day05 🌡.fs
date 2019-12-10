module Day05
let day = "05"

open System.IO

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let compile (str: string) = str.Split "," |> Array.map int
let program = compile lines.[0]

let memory = Array.copy program
let reset ()  = program |> Array.iteri (fun i v -> memory.[i] <- v)

let mutable systemId = -1
let readInput () = systemId

let mutable lastOutput = -1
let writeOutput value = lastOutput <- value

let read addr = memory.[addr]
let write addr value = memory.[addr] <- value

let halt ptr = read ptr = 99

let posDE n = n % 100
let posC n = n % 1000 / 100
let posB n = n % 10000 / 1000
let posA n = n % 100000 / 10000

let opCode = read >> posDE

let arg1 ptr =
    match (read >> posC) ptr with
    | 0 -> read (read (ptr + 1))
    | 1 -> read (ptr + 1)
    | u -> failwithf "Unexpected mode for first para: %i (ptr: %i)" u ptr

let arg2 ptr =
    match (read >> posB) ptr with
    | 0 -> read (read (ptr + 2))
    | 1 -> read (ptr + 2)
    | u -> failwithf "Unexpected mode for second para: %i (ptr: %i)" u ptr

let arg3 ptr =
    match (read >> posA) ptr with
    | 0 -> read (read (ptr + 3))
    | 1 -> read (ptr + 3)
    | u -> failwithf "Unexpected mode for thrid para: %i (ptr: %i)" u ptr

let add ptr =
    (write (read (ptr + 3)) (arg1 ptr + arg2 ptr))
    (ptr + 4)

let mult ptr =
    (write (read (ptr + 3)) (arg1 ptr * arg2 ptr))
    (ptr + 4)

let input ptr =
    (write (read (ptr + 1)) (readInput ()))
    (ptr + 2)

let output ptr =
    writeOutput (arg1 ptr)
    (ptr + 2)

let jumpIfTrue ptr =
    if (arg1 ptr) <> 0 then (arg2 ptr) else (ptr + 3)

let jumpIfFalse ptr =
    if (arg1 ptr) = 0 then (arg2 ptr) else ptr + 3

let lessThan ptr =
    write
        (read (ptr + 3))
        (if (arg1 ptr) < (arg2 ptr) then 1 else 0)
    (ptr + 4)

let equals ptr =
    write
        (read (ptr + 3))
        (if (arg1 ptr) = (arg2 ptr) then 1 else 0)
    (ptr + 4)

let operation ptr =
    match opCode ptr with
    | 1 -> add
    | 2 -> mult
    | 3 -> input
    | 4 -> output
    | 5 -> jumpIfTrue
    | 6 -> jumpIfFalse
    | 7 -> lessThan
    | 8 -> equals
    | u -> failwithf "Unexpected opCode: %i (ptr: %i)" u ptr

let run sysId =
    systemId <- sysId
    let rec execute ptr =
       if halt ptr then lastOutput else execute ((operation ptr) ptr)
    execute 0

let Part1 () =
    run 1

let Part2 () =
    reset ()
    run 5
