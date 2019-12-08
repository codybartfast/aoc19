module Day07
let day = "07"

open System.IO

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj
let nPrint = id

let rec permutations list =
    let rec insertAlong i list =
        match list with
        | [] -> [[i]]
        | h::t ->
            (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let compile (str: string) = str.Split "," |> Array.map int
let mutable program = compile lines.[0]

let mutable phaseSettings = [||]

let mutable outputA = -1
let mutable outputB = -1
let mutable outputC = -1
let mutable outputD = -1
let mutable outputE = -1

let reset () =
    outputA <- -1;
    outputB <- -1;
    outputC <- -1;
    outputD <- -1;
    outputE <- -1;

let computer readInput writeOutput =
    let mutable wroteOuput = false
    let mutable halted = false

    let memory = Array.copy program
    let reset ()  = program |> Array.iteri (fun i v -> memory.[i] <- v)

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

    let run () =
        wroteOuput <- false
        let rec execute ptr =
            if halt ptr then halted <- true; halted
           // elif wroteOuput then halted
            else execute ((operation ptr) ptr)
        if halted then halted else execute 0
    run

let readA () = if outputA = -1 then phaseSettings.[0] else outputE
let writeA value = outputA <- value

let readB () = if outputB = -1 then phaseSettings.[1] else outputA
let writeB value = outputB <- value

let readC () = if outputC = -1 then phaseSettings.[2] else outputB
let writeC value = outputC <- value

let readD () = if outputD = -1 then phaseSettings.[3] else outputC
let writeD value = outputD <- value

let readE () = if outputE = -1 then phaseSettings.[4] else outputD
let writeE value = outputE <- value

let run5 settings =
    reset ()
    phaseSettings <- Array.ofList settings
    let runA = computer readA writeA
    let runB = computer readB writeB
    let runC = computer readC writeB
    let runD = computer readD writeD
    let runE = computer readE writeE
    runA ();
    runB (); 
    runC (); 
    runD (); 
    runE ();
    outputE

let test1 = @"3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

let Part1 () =
    // ()
    run5  [4; 3; 2; 1; 0]
    // [0; 1; 2; 3; 4]
    // |> permutations
    // |> List.map run5
    // |> List.max

let Part2 () =
    //program <- compile test1
    ()