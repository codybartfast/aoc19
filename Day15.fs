module Day15
let day = "15"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let mapToString (map:Map<(int * int), char>) droid =
    // aoc18:20
    let valueAsChar = function
        | None -> '?'
        | Some chr -> chr
        // | u -> failwithf "Unexpected value: %O" u

    let coords = map |> Map.toList |> List.map fst
    let minX = coords |> (List.map fst >> List.min)
    let maxX = coords |> (List.map fst >> List.max)
    let minY = coords |> (List.map snd >> List.min)
    let maxY = coords |> (List.map snd >> List.max)
    let (width, height) = (1 + maxX-minX, 1 + maxY-minY)
    let (xShift, yShift) = ((+) minX), ((+) minY)

    Array.init (height)  (fun  y ->
        Array.init (width) (fun x ->
            let x, y = xShift x, yShift y
            if (x, y) = droid
            then 'D'
            // elif (x, y) = (0, 0)
            // then 'X'
            // elif (x, y) = oxygen
            // then 'O'
            else map.TryFind (x, y) |> valueAsChar))
    |> Array.map String
    |> String.concat nl

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

    let runToHalt () =
        let rec run ptr =
            if halt ptr then false else run ((operation ptr) ptr)
        run 0

    program |> Array.iteri write    
    
    runToOutput

let droidApi () =
    let mutable instr = -1
    let mutable resp = -1

    let provideInput = fun () -> int64 instr
    let handleOutput out = resp <- int out 

    let sendInstr run instruction =
        instr <- instruction
        run () |> ignore
        resp
    
    provideInput, handleOutput, sendInstr

let step (x, y) dir =
    match dir with
    | 1 -> (x, y - 1)
    | 2 -> (x, y + 1)
    | 3 -> (x - 1, y)
    | 4 -> (x + 1, y)
    | _ -> failwith "jump?" 

let mapWall sendInstr map pos dir =
    // if sendInstr dir <> 0 then failwith "expected a wall"
    
    let right = function
        1 -> 4 | 2 -> 3 | 3 -> 1 | 4 -> 2 | _ -> failwith "really?"
    let left = function
        1 -> 3 | 2 -> 4 | 3 -> 2 | 4 -> 1 | _ -> failwith "really?"
    
    let rec move map previous =
        let pos, dir = previous
        let resp = sendInstr dir
        let target = step pos dir        
        match resp with
        | 0 -> 
            let newDir = right dir
            move
                (Map.add target '#' map)
                (pos, newDir)
        | 1 ->
            let newDir = left dir
            let map =
                if map.ContainsKey target
                then map
                else Map.add target '.' map
            move map (target, newDir)
        | 2 -> 
            if map.ContainsKey target then
                map
            else
                let newDir = left dir
                let map = Map.add target 'O' map
                // print (mapToString map pos)
                move
                    map
                    (target, newDir)            
        | _ ->  failwith "time to reboot?"

    move map (pos, dir)

let rec explore sendInstr map pos =
    Console.Clear()
    print "-------------------------------------------------------------"
    printfn "Position: %A" pos
    print (mapToString map pos)

    let target (x, y) = function
        | 1 -> (x, y - 1)
        | 2 -> (x, y + 1)
        | 3 -> (x - 1, y)
        | 4 -> (x + 1, y)
        | _ -> failwith "jump?" 
    
    let right = function
        1 -> 4 | 2 -> 3 | 3 -> 1 | 4 -> 2 | _ -> failwith "really?"
    let left = function
        1 -> 3 | 2 -> 4 | 3 -> 2 | 4 -> 1 | _ -> failwith "really?"

    let info = Console.ReadKey(true)
    let dir = 
        match info.Key with
        | ConsoleKey.UpArrow -> 1
        | ConsoleKey.DownArrow -> 2
        | ConsoleKey.RightArrow -> 4
        | ConsoleKey.LeftArrow -> 3
        | _ -> -1    
    
    if dir = -1 then explore sendInstr map pos
    printfn "key: %A" dir
    let target = target pos dir

    let resp = sendInstr dir

    let map, pos = 
        match resp with
        | 0 -> (Map.add target '#' map), pos
        | 1 -> 
            let map =
                if map.ContainsKey target
                then map
                else Map.add target '.' map
            map, target
        | 2 -> (Map.add target 'O' map), target
        | _ -> failwith "time to roboot?"

    printfn "Response: %i, Position %A, Target %A" resp pos target
    
    explore sendInstr map pos

let rec findRoute map  =
    let rec allRoutes pos dir count =
        match Map.find pos map with
        | 'O' -> Seq.singleton (Some count)
        | '#' -> Seq.singleton None
        | '.' ->
            let reverse = function 
                | -1 -> -1 | 1 -> 2 | 2 -> 1 | 3 -> 4 | 4 -> 3
                | u -> failwithf "is this a direction?: %i" u
            let cameFrom = reverse dir
            [1; 2; 3; 4]
            |> Seq.filter ((<>) cameFrom)
            |> Seq.collect (fun dir ->
                allRoutes (step pos dir) dir (count + 1))
        | u -> failwithf "I'm standing on this... %A" u
    allRoutes (0, 0) -1 0
    |> Seq.choose id
    |> Seq.head
        
        





let Part1 () =
    let program = compile code
    let readInput, writeOutput, sendInstr = droidApi ()
    let run = computer program readInput writeOutput
    let sendInstr = sendInstr run

    let dir = 1
    let map = Map.add (0,0) 'X' Map.empty
    let map = mapWall sendInstr map (0, 0) dir
    print (mapToString map (Int32.MinValue, Int32.MinValue))
    let map = Map.add (0,0) '.'map
    print (findRoute map)
    Console.ReadKey()


let Part2 () =
    ()
