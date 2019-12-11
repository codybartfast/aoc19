module Day11
let day = "11"

open System
open System.IO

let nl = System.Environment.NewLine

let computer program readInput writeOutput =
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

    let runToHalt () =
        let rec run ptr =
            if halt ptr then false else run ((operation ptr) ptr)
        run 0

    program |> Array.iteri write
    runToHalt

let mapToString (map:Map<(int * int), int64>) =
    let valueAsString = function
        | None | Some 0L -> ' '
        | Some 1L -> '█'
        | u -> failwithf "Unexpected value: %O" u

    let coords = map |> Map.toList |> List.map fst
    let minX = coords |> (List.map fst >> List.min)
    let maxX = coords |> (List.map fst >> List.max)
    let minY = coords |> (List.map snd >> List.min)
    let maxY = coords |> (List.map snd >> List.max)
    let (width, height) = (1 + maxX-minX, 1 + maxY-minY)
    let (xShift, yShift) = ((+) minX), ((+) minY)

    Array.init (height)  (fun  y ->
        Array.init (width) (fun x ->
            map.TryFind (xShift x, yShift y) |> valueAsString))
    |> Array.map String
    |> String.concat nl

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

type Direction = U | R | D | L
type Robot =
    { Coord : int * int
      Direction: Direction
      History: Map<(int * int), int64>}
let (black, white) = 0L, 1L

let currentColour robot =
    match robot.History.TryFind (robot.Coord) with
    | None -> 0L
    | Some col -> col

let paintPanel robot (colour, turnDir)=
    let paint robot =
        { robot with History = robot.History.Add (robot.Coord, colour) }
    let turn robot =
        let newDirection =
            match robot.Direction, turnDir with
            | U, 1L -> R | R, 1L -> D | D, 1L -> L | L, 1L -> U
            | U, 0L -> L | L, 0L -> D | D, 0L -> R | R, 0L -> U
            | _ -> failwith "Oh! Oh! Oh!"
        { robot with Direction = newDirection}
    let move robot =
        let newCoord =
            let (x, y) = robot.Coord
            match robot.Direction with
            | U -> x, y - 1 | R -> x + 1, y | D -> x, y + 1 | L -> x - 1, y
        { robot with Coord = newCoord }
    robot |> paint |> turn |> move

let robotApi startCol =
    let mutable robbie =
        { Coord = (0, 0)
          Direction = U
          History = [(0, 0), startCol] |> Map }
    let mutable received = []

    let provideInput () = currentColour robbie

    let handleOutput out =
        received <- out::received
        match received with
        | [_] -> ()
        | [turnDir; colour] ->
            robbie <- paintPanel robbie (colour, turnDir)
            received <- []
        | _ -> failwith "He's making a list, he's checking it twice"

    provideInput, handleOutput, fun () -> robbie.History

let goPaint startColour =
    let program = compile code
    let readInput, writeOutput, getHistory = robotApi startColour
    let run = computer program readInput writeOutput
    run () |> ignore
    getHistory ()

let Part1 () = goPaint black |> Map.count

let Part2 () = sprintf "%s%s%s" nl (mapToString (goPaint white)) nl
