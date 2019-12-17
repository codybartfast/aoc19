module Day17
let day = "17"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let computer program readInput writeOutput mode =
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
    write 0 mode

    runToHalt

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(y) = this.FormatRow (this.Row(y))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this

    member this.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    // member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.FilterSeq(v) =
        this.Coords ()
        |> Seq.filter (fun (x, y) -> this.[x, y] = v)

    member this.NHood(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                let inb = this.InBounds (x, y) //////////////////////////
                if this.InBounds (x, y)
                then Some this.[x,y]
                else None |]

    member this.Adjacent(x, y) =
        let nhood = this.NHood (x, y)
        nhood.[4] <- None
        nhood

    member this.Transform(generate: Grid<'a> -> int -> int -> 'b) : Grid<'b>=
        [| for y in 0 .. maxY do
            [| for x in 0 .. maxX do
                generate this x y |] |]
        |> Grid<'b>

    member _.Flattern() = Array.collect id data
    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
    member this.Get((x, y)) = this.[x, y] ////////////////////   ////////
    member this.Set((x, y)) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

type Thingy = Grid<char>

let textGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Thingy

let cameraApi () =
    let mutable received = []

    let input () = failwith "Uh well, um, ... zero?"
    let output out = received <- out::received

    input, output, (fun () -> List.rev received)

let readScaffold scaffold =
    scaffold ()
    |> List.map char
    |> Array.ofList
    |> String

type Turn = Left | Right | DeadEnd
type Direction = North | South | West | East
let right = function
    | North -> East | South -> West | West -> North | East -> South
let left = function
    | North -> West | South -> East | West -> South | East -> North

let map (grid: Thingy) =
    let start =
        grid.Coords ()
        |> Seq.map (fun coord -> (coord, grid.Get coord))
        |> Seq.find (fun (_, char) -> char = '^')
        |> fst

    let next (x, y) dir =
        match dir with
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        | East -> (x + 1, y)

    let rec walk (grid: Thingy) pos dir =
        let next = next pos dir
        match grid.TryGet(next) with
        | Some '#' -> walk grid next dir
        | _ -> pos        

    let rec turnAndGo (grid: Thingy) path pos dir =
        let onLeft = grid.TryGet (left dir |> (next pos))
        let onRight = grid.TryGet (right dir |> (next pos))
        let turn =
            match onLeft, onRight with
            | Some '#', _ -> Left
            | _, Some '#' -> Right
            | _ -> DeadEnd
        if turn = DeadEnd then List.rev path else
            let dir' = match turn with Left -> left dir | Right -> right dir
            let pos' = walk grid pos dir'
            let manhatten (x, y) (x', y') = abs (x' - x) + abs (y' - y)
            let dist = manhatten pos pos'
            let turn = match turn with Left -> "L" | Right -> "R"
            let instr = sprintf "%s,%i" turn dist
            turnAndGo grid (instr::path) pos' dir'

    turnAndGo grid [] start North

       


let mutable path = []


let Part1 () =
    let program = compile code
    let input, output, scaffold = cameraApi ()
    let run = computer program input output 1L
    run () |> ignore
    let scaffold = readScaffold scaffold
    let grid = textGrid (scaffold.TrimEnd().Split "\n")
    path <- map grid
    (grid.Transform (fun grid x y ->
        let nHood = grid.NHood (x, y)
        match nHood.[4] with
        | None -> 0
        | Some '.' -> 0
        | Some '^' | Some '>' | Some 'v' | Some '<' -> 0
        | Some '#' ->
            let isIntersection =
                [nHood.[1]; nHood.[3]; nHood.[5]; nHood.[7] ]
                |> List.forall (fun n -> n = Some '#')
            if isIntersection then x * y else 0)
    ).Flattern ()
    |> Array.sum

let rescueApi rules =
    let mutable rules = rules
    let mutable received = []

    let input () =
        let head::tail = rules
        rules <- tail
        // printf "Input: "; print head
        head
    let output out = received <- out::received

    input, output, (fun () -> received)

let intRules (main, a, b, c, feed) =
    [main; a; b; c; feed; ""]
    |> String.concat "\n"
    |> (fun (str: string) -> (tPrint str).ToCharArray())
    |> List.ofArray
    |> List.map int64

let demoRules =
    let main = "A,B,C,B,A,C"
    let a = "R,8,R,8"
    let b = "R,4,R,4,R,8"
    let c = "L,6,L,2"
    let feed = "n"
    (main, a, b, c, feed)

let rules =                     //
    let main = "A,B,A,B,C,A,B,C,A,C"
    let a = "R,6,L,10,R,8"
    let b = "R,8,R,12,L,8,L,8"
    let c = "L,10,R,6,R,6,L,8"
    let feed = "n"
    (main, a, b, c, feed)


let Part2 () =
    let program = compile code
    let intRules = intRules rules
    let input, output, received = rescueApi intRules
    let run = computer program input output 2L
    run () |> ignore

    // print (String.concat "," path)

    received ()
    |> List.filter (fun n -> n > 255L)
