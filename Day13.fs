module Day13
let day = "13"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let computer program readInput writeOutput quarters =
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
    match quarters with
    | None -> ()
    | Some q -> write 0 q
    runToHalt

let mapToString (map:Map<(int * int), int64>) =
    // aoc18:20
    let valueAsString = function
        | None | Some 0L -> ' '
        | Some 1L-> '█'
        | Some 2L -> 'X'
        | Some 3L -> '_'
        | Some 4L -> 'o'
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


let empty = 0L
let wall = 1L
let block = 2L
let paddle = 3L
let ball = 4L

let left = -1L
let stay = 0L
let right = 1L

let paddleRow = 20
let ballLow = paddleRow - 1

let target (x, y) (x', y') =
    let goingUp = y > y'
    let goingRight = x < x'
    let ball = x'

    if y' = ballLow then x' else
    
    if goingUp 
    then x'
    else if goingRight then ball + 1 else ball - 1


let mutable moves = 0
let chooseMove prevBall ball paddle =
    printfn "Move %i: %A" moves (prevBall, ball, paddle)
    moves <- moves + 1
    
    // print (goingUp, goingRight)

    let target = target prevBall ball
        // if goingUp then ball
        // elif goingRight  then ball + 1 else ball - 1

    if paddle < target then right elif target < paddle then left else stay
        

    

let player1 =
    let mutable prevBall = 15, 16
    fun screen ->
        // screen |> mapToString |> printfn "%s"


        let tiles = Map.toList screen
        let find kind =
            tiles |> List.filter (snd >> (=) kind) |> List.head |> fst
        let paddle = find paddle |> fst
        let ball' = find ball 

        let ball = prevBall
        prevBall <- ball'

        chooseMove ball ball' paddle
    
    

let screenApi () =
    let mutable screen = Map.empty
    let mutable score = -1L
    let mutable received = []

    let provideInput () = player1 screen

    let handleOutput out =
        received <- out::received
        match received with
        | [_] -> ()
        | [_; _] -> ()
        | [s; 0L; -1L] ->
            score <- s
            received <- []
        | [tile; y; x] ->
            screen <- (screen.Add ((int x, int y), tile))
            received <- []
        | _ -> failwith "He's making a list, he's checking it twice"

    provideInput, handleOutput, fun () -> (screen, score)
    
let Part1 () = 
    let program = compile code
    let readInput, writeOutput, getScreen = screenApi ()
    let run = computer program readInput writeOutput None
    run () |> ignore
    getScreen () 
    |> fst
    |> Map.toSeq 
    |> Seq.map snd
    |> Seq.filter ((=) block) 
    |> Seq.length

let Part2 () =
    let program = compile code
    let readInput, writeOutput, getScreen = screenApi ()
    let run = computer program readInput writeOutput (Some 2L)
    run () |> ignore
    let (screen, score) = getScreen () 
    score
