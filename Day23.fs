module Day23
let day = "23"

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%A" obj)
let tPrint obj = (print obj); obj

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let computer program readInput writeOutput =
    let mutable running = true
    let mutable wroteOutput = false
    let mutable ptrOnPause = 0
    let mutable relBase = 0

    let memMB = 1
    let memory = Array.init (memMB * 1_024 * 1_024) (fun _ -> 0L)
    let read (addr: int) = memory.[int addr]
    let write (addr: int) (value: int64) = memory.[addr] <- value

    let halt ptr = read ptr = 99L

    let posDE n = n % 100
    let posC n = n % 1000 / 100
    let posB n = n % 10000 / 1000
    let posA n = n % 100000 / 10000

    let opCode = read >> int >> posDE

    let readArg offset modeFlag ptr =
        let addr = ptr + offset
        match ptr |> (read >> int >> modeFlag) with
        | 0 -> read (int (read addr))
        | 1 -> read addr
        | 2 -> read ((int (read addr)) + relBase)
        | u -> failwithf "Unexpected mode flag: %i (ptr: %i)" u ptr
    let arg1 = readArg 1 posC
    let arg2 = readArg 2 posB
    let arg3 = readArg 3 posA

    let writeArg offset modeFlag ptr value =
        let addr = ptr + offset
        match ptr |> (read >> int >> modeFlag) with
        | 0 -> write (int (read addr)) value
        | 1 -> failwith "Oh! Oh! Oh!"
        | 2 -> write ((int (read addr)) + relBase) value
        | u -> failwithf "Unexpected mode flag: %i (ptr: %i)" u ptr
    let write1 = writeArg 1 posC
    let write2 = writeArg 2 posB
    let write3 = writeArg 3 posA

    let add ptr = arg1 ptr + arg2 ptr |> write3 ptr;  ptr + 4
    let mult ptr = arg1 ptr * arg2 ptr |> write3 ptr;  ptr + 4
    let input ptr = readInput () |> write1 ptr;  ptr + 2
    let output ptr = writeOutput (arg1 ptr);  ptr + 2
    let jumpIfTrue ptr = if arg1 ptr <> 0L then int (arg2 ptr) else ptr + 3
    let jumpIfFalse ptr = if arg1 ptr = 0L then int (arg2 ptr) else ptr + 3
    let lessThan ptr =
        (if arg1 ptr < arg2 ptr then 1L else 0L) |> write3 ptr;  ptr + 4
    let equals ptr =
        (if arg1 ptr = arg2 ptr then 1L else 0L) |> write3 ptr;  ptr + 4
    let shiftRB ptr = relBase <- relBase + (int (arg1 ptr));  ptr + 2

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
        | 9 -> shiftRB
        | u -> failwithf "Unexpected opCode: %i (ptr: %i)" u ptr

    let step () =
        if running then
            let ptr = ptrOnPause
            if halt ptr 
            then running <- false
            else ptrOnPause <- ((operation ptr) ptr)
            running
        else
            running

    let runToOutput () =
        let rec run ptr =
            if not running then false
            elif wroteOutput then ptrOnPause <- ptr;  true
            elif halt ptr then running <- false; false
            else run ((operation ptr) ptr)
        wroteOutput <- false
        run ptrOnPause

    let runToHalt () =
        let rec run ptr =
            if halt ptr then false else run ((operation ptr) ptr)
        run 0

    program |> Array.iteri write
    step

let networkComputer program address =
    let inBuff = Queue<int64> ()
    let outBuff = Queue<int64> ()

    let write = inBuff.Enqueue
    let provideInput () = 
        match inBuff.TryDequeue () with
        | false, _ -> -1L
        | true, item -> item
       
    let handleOutput = outBuff.Enqueue
    let read () =
        if outBuff.Count >= 3 then
            Some (outBuff.Dequeue (), 
                    (outBuff.Dequeue (), outBuff.Dequeue ()))
        else
            None

    write address
    let step = computer program provideInput handleOutput
    (step, read, write)

let computers program =
    [0L .. 49L]
    |> List.map (fun i -> i, networkComputer program i)
    |> Map

let run (computers) cycles =
    let cycles = [1 .. cycles]
    computers
    |> Map.toList
    |> List.iter (fun (_, (step, _, _)) ->
       cycles |> List.iter (fun _ -> step () |> ignore) ) 

let sendPackets computers =
    let sent =
        computers
        |> Map.toList
        |> List.map (fun (source, (_, read, _)) -> read ())
        |> List.choose id
    match List.tryFind (fst >> ((=) 255L)) sent with
    | Some (_, (_, y)) -> Some y
    | None ->
        sent
        |> List.iter (fun (addr, (x, y)) ->
            let _, _, write = computers.[addr]
            write x
            write y )
        None

let rec runSend computers cycles =
    run computers cycles
    match sendPackets computers with
    | Some y -> y
    | None -> runSend computers cycles

let Part1 () =
    let computers = computers (compile code)
    runSend computers 50



let Part2 () =
    ()
