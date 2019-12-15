module Day15
let day = "15"

#nowarn "0025"

open System.IO

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let computer program readInput writeOutput =
    let mutable wroteOutput = false
    let mutable ptrOnPause = 0
    let mutable running = true

    let writeOutput value = wroteOutput <- true; writeOutput value

    let mutable relBase = 0

    let memKB = 1_024
    let memory = Array.init (memKB * 1_024) (fun _ -> 0L)
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

    let runToOutput () =
        let rec run ptr =
            if not running then false
            elif wroteOutput then ptrOnPause <- ptr;  true
            elif halt ptr then running <- false; false
            else run ((operation ptr) ptr)
        wroteOutput <- false
        run ptrOnPause

    program |> Array.iteri write

    runToOutput

let controlInverter () =
    let mutable instr = -1
    let mutable resp = -1

    let provideInput = fun () -> int64 instr
    let handleOutput out = resp <- int out
    let sendInstr run instruction =
        instr <- instruction
        run () |> ignore
        resp

    provideInput, handleOutput, sendInstr

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let computerApi sendInst =
    fun command ->
        let instr =
            match command with
            North -> 1 | South -> 2 | West -> 3 | East -> 4
        match sendInst instr with
        | 0 -> Wall
        | 1 -> Clear
        | 2 -> Oxygen

let nextPosition (x, y) dir =
    match dir with
    | North -> (x, y - 1)
    | South -> (x, y + 1)
    | West -> (x - 1, y)
    | East -> (x + 1, y)

let right = function
    | North -> East | South -> West | West -> North | East -> South
let left = function
    | North -> West | South -> East | West -> South | East -> North
let reverse = function
    | North -> South | South -> North | West -> East | East -> West

let mapWall sendCommand map pos dir =
    let rec move map previous =
        let pos, dir = previous
        let resp = sendCommand dir
        let target = nextPosition pos dir
        match resp with
        | Wall -> move (Map.add target Wall map) (pos, right dir)
        | Clear ->
            match map.TryFind target with
            | Some Origin -> map
            | _ -> move (Map.add target Clear map) (target, left dir)
        | Oxygen -> move (Map.add target Oxygen map) (target, left dir)
    move map (pos, dir)

let distances map target startPosition =
    let rec distances pos dir dist =
        match Map.find pos map with
        | loc when loc = target ->  Seq.singleton (Some dist)
        | Wall -> Seq.singleton None
        | Clear | Oxygen | Origin ->
            let cameFrom = reverse dir
            [North; South; West; East]
            |> Seq.filter ((<>) cameFrom)
            |> Seq.collect (fun dir ->
                distances (nextPosition pos dir) dir (dist + 1))
    distances startPosition North 0

let sectionMap =
    let program = compile code
    let readInput, writeOutput, sendInstr = controlInverter ()
    let run = computer program readInput writeOutput
    let sendCommand = computerApi (sendInstr run)
    let map = Map.add (0,0) Origin Map.empty
    // North works in this instance but generally needs to be
    // coming from a direction with a wall
    mapWall sendCommand map (0, 0) North

let Part1 () =
    distances sectionMap Oxygen (0, 0)
    |> Seq.pick id

let Part2 () =
    let oxygenLocation =
        sectionMap
        |> Map.toSeq
        |> Seq.find (snd >> ((=) Oxygen))
        |> fst
    distances sectionMap Wall oxygenLocation
    |> Seq.choose id
    |> Seq.max
    |> ((+) -1)
