module Day11
let day = "11"

open System
open System.IO
open System.Text.RegularExpressions

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

    let writeOutput value = wroteOutput <- true; writeOutput value

    let memKB = 1_024
    let memory = Array.init (memKB * 1_024) (fun _ -> 0L)
    program |> Array.iteri (fun i v -> memory.[i] <- v)
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

    runToHalt

type Dir = U | R | D | L
type Robot = (int * int) * Dir * Map<(int * int), int>
let black = 0
let white = 1

let readColour (robot: Robot) =
    let coord,  _, panel = robot
    match panel.TryFind coord with
    | None -> black
    | Some col -> col

let act (robot: Robot) (col, turn) =
    let paint (robot: Robot) =
        let coord, dir, panel = robot
        coord, dir, panel.Add (coord, col)
    let turn robot =
        let coord, dir, panel = robot
        let dir = 
            match dir, turn with
            | U, 1 -> R | R, 1 -> D | D, 1 -> L | L, 1 -> U
            | U, 0 -> L | L, 0 -> D | D, 0 -> R | R, 0 -> U
            | _ -> failwith "Oh! Oh! Oh!"
        coord, dir, panel
    let move robot = 
        let (x, y), dir, panel = robot
        let coord =
            match dir with
            | U -> x, y - 1 | R -> x + 1, y | D -> x, y + 1 | L -> x - 1, y
        coord, dir, panel
    robot |> paint |> turn |> move

let robbie () =
    let mutable robbie = (0, 0), U, Map.empty    
    let provideInput () = readColour robbie

    let mutable received = []
    let handleOutput out =
        received <- out::received
        match received with
        | [_] -> ()
        | [dir; col] -> 
            robbie <- act robbie (col, dir)
            received <- []
        | _ -> failwith "He's making a list, he's checking it twice"
    
    provideInput, handleOutput, fun () -> robbie

let Part1 () =
    let program = compile code
    let readInput, writeOutput, getRobbie = robbie ()
    let run = computer program (readInput >> int64) (int >> writeOutput)
    run () |> ignore
    let _, _, panel = getRobbie ()
    panel.Count


let Part2 () =
    ()
