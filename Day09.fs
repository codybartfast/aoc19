module Day09
let day = "09"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64
let mutable program = compile code

let computer program readInput writeOutput =
    let mutable running = true
    let mutable wroteOutput = false
    let mutable ptrOnPause = 0
    let mutable relBase = 0

    let writeOutput value = wroteOutput <- true; writeOutput value

    let memKB = 1_000
    let memory = Array.init (memKB * 1024) (fun _ -> 0L)
    program |> Array.iteri (fun i v -> memory.[i] <- v)
    let read (addr: int) = memory.[int addr]
    let write (addr: int) (value: int64) = memory.[addr] <- value

    let halt ptr = read ptr = 99L

    let posDE n = (int n) % 100 
    let posC n = (int n) % 1000 / 100
    let posB n = (int n) % 10000 / 1000
    let posA n = (int n) % 100000 / 10000

    let opCode = read >> posDE

    let readArg offset modeFlag ptr =
        let addr = ptr + offset
        match ptr |> (read >> modeFlag) with
        | 0 -> read (int (read addr))
        | 1 -> read addr
        | 2 -> read ((int (read addr)) + relBase)
        | u -> failwithf "Unexpected mode flag: %i (ptr: %i)" u ptr

    let arg1 = readArg 1 posC
    let arg2 = readArg 2 posB
    let arg3 = readArg 3 posA

    let writeArg offset modeFlag ptr value =
        let addr = ptr + offset
        match ptr |> (read >> modeFlag) with
        | 0 -> write (int (read addr)) value
        | 1 -> failwithf "Oh! Oh! Oh!"
        | 2 -> write ((int (read addr)) + relBase) value
        | u -> failwithf "Unexpected mode flag: %i (ptr: %i)" u ptr
    
    let write1 = writeArg 1 posC
    let write2 = writeArg 2 posB
    let write3 = writeArg 3 posA

    let add ptr = write3 ptr (arg1 ptr + arg2 ptr); (ptr + 4)
    let mult ptr = write3 ptr (arg1 ptr * arg2 ptr); (ptr + 4)
    let input ptr = write1 ptr (readInput ()); (ptr + 2)
    let output ptr = writeOutput (arg1 ptr); (ptr + 2)
    let jumpIfTrue ptr = if (arg1 ptr) <> 0L then (int (arg2 ptr)) else (ptr + 3)
    let jumpIfFalse ptr = if (arg1 ptr) = 0L then (int (arg2 ptr)) else ptr + 3

    let lessThan ptr =
        write3 ptr (if (arg1 ptr) < (arg2 ptr) then 1L else 0L)
        (ptr + 4)

    let equals ptr =
        write3 ptr (if (arg1 ptr) = (arg2 ptr) then 1L else 0L)
        (ptr + 4)

    let shiftRB ptr =
        relBase <- relBase + (int (arg1 ptr)); ptr + 2

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

    let run () =
        while step () do ()
        false

    run

let day5 =
    Path.Combine("inputs", "input" + "05" + ".txt")
    |> File.ReadAllLines
    |> Array.head

let test1a = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
let test1b = "1102,34915192,34915192,7,4,7,99,0"
let test1c = "104,1125899906842624,99"

let run code =
    program <- compile code
    let input () = 1L
    let mutable result = []
    let output value = result <- value::result
    (computer program input output) () |> ignore
    List.rev result

let Part1 () =
    run code;


let Part2 () =
    ()
