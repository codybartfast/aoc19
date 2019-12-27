module Day21
let day = "21"

open System
open System.IO

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let computer program readInput writeOutput =
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

    let runToHalt () =
        let rec run ptr =
            if halt ptr then false else run ((operation ptr) ptr)
        run 0

    program |> Array.iteri write

    runToHalt

let controlInverter () =
    let mutable (inputList: int64 list) = []
    let mutable (resp: int64 list) = []

    let provideInput = fun () ->
        let input = List.head inputList
        inputList <- List.tail inputList
        input
    let handleOutput out = resp <- (out::resp)
    let sendInstr run inputs =
        inputList <- inputs
        run () |> ignore
        List.rev resp

    provideInput, handleOutput, sendInstr

let springApi sendInstr mode =
    fun instructions ->
        instructions
        |> String.concat "\n"
        |> fun str -> str + sprintf "\n%s\n" mode
        |> fun str -> str.ToCharArray()
        |> Array.map int64
        |> List.ofArray
        |> sendInstr
        |> fun list ->
            if List.length list = 34 then
                (list.Item 33).ToString()
            else
                list
                |> Array.ofList
                |> Array.map char
                |> String

let spring mode =
    let program = compile code
    let input, output, sendInstr = controlInverter ()
    let run = computer program input output
    springApi (sendInstr run) mode

let Part1 () =
    spring "WALK"
        [
            "NOT A J"   // J: A is gap
            "NOT B T"   // T: B is gap
            "OR T J"    // J: A or B is gap
            "NOT C T"   // T: C is gap
            "OR T J"    // J: A, B or C s gap
            "AND D J"   // ... and D is present so jump
        ]

let Part2 () =
    spring "RUN"
        [
            "OR D T"
            "AND H T"   // T: can Jump twice

            "OR D J"
            "AND E J"
            "AND I J"   // J: can Jump, Step then Jump

            "OR T J"    // J: Looks safe to jump if have to

            "OR J T"    // path only matters if J is true, so assume it is
            "AND B T"   // T: true if B present
            "AND C T"   // T: true if B and C present
            "NOT T T"   // T: true if B or C missing

            "AND T J"   // upcoming gap and looks safe

            "NOT A T"   // if A missing don't think, jump

            "OR T J"    // jump if A gap or, B or C gap and it looks safe
        ]
