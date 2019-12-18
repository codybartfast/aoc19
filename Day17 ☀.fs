module Day17
let day = "17"

#nowarn "0025"

open System
open System.IO

let readLines day =
    Path.Combine("inputs", "input" + day + ".txt")
    |> File.ReadAllLines
let lines = readLines day
let code = lines.[0]
let compile (str: string) = str.Split "," |> Array.map int64

let computer program readInput writeOutput mode =
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
    write 0 mode

    runToHalt

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.Filter(pred) =
        this.Coords ()
        |> Seq.filter (fun (x, y) -> (pred this.[x, y]))

    member this.Bordering(x, y) =
        [| this.TryGet (x, y - 1);
           this.TryGet (x + 1, y);
           this.TryGet (x, y + 1);
           this.TryGet (x - 1, y); |]

    member this.Flatern() =
        this.Coords ()
        |> Seq.map (fun (x, y) -> (x, y), this.[x, y])

    member this.Get((x, y)) = this.[x, y]
    member this.Set((x, y)) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

type Scaffold = Grid<char>

let cleanerApi rules =
    let mutable rules = rules
    let mutable received = []
    let input () =
        let head::tail = rules
        rules <- tail
        head
    let output out = received <- out::received
    input, output, (fun () -> List.rev received)

let scaffoldGrid =
    List.map char
    >> Array.ofList
    >> String
    >> (fun str -> str.Trim().Split "\n")
    >> Array.map (fun str -> str.ToCharArray())
    >> Scaffold

let scaffold =
    let program = compile code
    let input, output, received = cleanerApi []
    let run = computer program input output 1L
    run () |> ignore
    scaffoldGrid (received ())

type Turn = Left | Right | DeadEnd
type Direction = North | South | West | East

let findPath (grid: Scaffold) =
    let start =
        grid.Filter ((=) '^')
        |> Seq.head

    let next (x, y) dir =
        match dir with
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        | East -> (x + 1, y)

    let rec walk pos dir =
        let next = next pos dir
        match grid.TryGet(next) with
        | Some '#' -> walk next dir
        | _ -> pos

    let rec turnAndWalk path pos dir =
        let right = function
            | North -> East | South -> West | West -> North | East -> South
        let left = function
            | North -> West | South -> East | West -> South | East -> North
        let onLeft = grid.TryGet (left dir |> (next pos))
        let onRight = grid.TryGet (right dir |> (next pos))
        let turn =
            match onLeft, onRight with
            | Some '#', _ -> Left
            | _, Some '#' -> Right
            | _ -> DeadEnd
        if turn = DeadEnd then List.rev path else
            let dir' = match turn with Left -> left dir | Right -> right dir
            let pos' = walk pos dir'
            let dst = abs (fst pos' - fst pos) + abs (snd pos' - snd pos)
            let trn = match turn with Left -> "L" | Right -> "R"
            let instr = sprintf "%s,%i" trn dst
            turnAndWalk (instr::path) pos' dir'

    turnAndWalk [] start North


let routines path =
    let matchGroups groups path =
        let rec useGroup group path =
            match group, path with
            | [], _ -> Some path
            | _, [] -> None
            | hg::tg, hp::tp when hg = hp -> useGroup tg tp
            | _ -> None
        let rec matchGroups' path lbls =
            groups
            |> List.map (fun (label, grp) -> label, useGroup grp path)
            |> List.tryFind (snd >> ((<>) None))
            |> function
                | None -> lbls, path
                | Some (lbl, Some []) -> (lbl::lbls), []
                | Some (lbl, Some path) -> matchGroups' path (lbl::lbls)
        let revPath, remaining = matchGroups' path []
        (List.rev revPath, remaining)

    let _, groups =
        seq{ for a in 1 .. 6 do
                let grpA = ("A", List.take a path)
                let (_, path) = matchGroups [grpA] path
                for b in 1 .. (min 6 path.Length) do
                    let grpB = ("B", List.take b path)
                    let (_, path) = matchGroups [grpA; grpB] path
                    for c in 1 .. (min 6 path.Length) do
                        let grpC = ("C", List.take c path)
                        (matchGroups [grpA; grpB; grpC] path),
                            [grpA; grpB; grpC] }
        |> Seq.find (fst >> snd >> ((=) []))

    let main = matchGroups groups path |> fst
    main::(groups |> List.map snd)
    |> List.map (String.concat ",")

let intRoutines [main; a; b; c] =
    [main; a; b; c; "n"; ""]
    |> String.concat "\n"
    |> (fun (str: string) -> str.ToCharArray())
    |> List.ofArray
    |> List.map int64

let Part1 () =
    scaffold.Flatern()
    |> Seq.filter (fun ((x, y), value) ->
        (value =  '#') &&
            (scaffold.Bordering(x, y) =
                [|Some '#'; Some '#'; Some '#'; Some '#'|]))
    |> Seq.sumBy (fst >> (fun (x, y) -> x * y))

let Part2 () =
    let path = findPath scaffold
    let intRoutines = intRoutines (routines path)
    let program = compile code
    let input, output, received = cleanerApi intRoutines
    let run = computer program input output 2L
    run () |> ignore
    received () |> List.max |> int
