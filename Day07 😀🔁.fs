module Day07
let day = "07"

open System.IO

let rec permutations list =
    let rec insertAlong i list =
        match list with
        | [] -> [[i]]
        | h::t ->
            (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let compile (str: string) = str.Split "," |> Array.map int
let program = compile lines.[0]

let computer program readInput writeOutput =
    let mutable wroteOutput = false
    let mutable ptrOnPause = 0
    let mutable running = true

    let writeOutput value = wroteOutput <- true; writeOutput value

    let memory = Array.copy program
    let read addr = memory.[addr]
    let write addr value = memory.[addr] <- value

    let halt ptr = read ptr = 99

    let posDE n = n % 100
    let posC n = n % 1000 / 100
    let posB n = n % 10000 / 1000
    let posA n = n % 100000 / 10000

    let opCode = read >> posDE

    let readArg offset modeFlag ptr =
        match ptr |> (read >> modeFlag) with
        | 0 -> read (read (ptr + offset))
        | 1 -> read (ptr + offset)
        | u -> failwithf "Unexpected mode flag: %i (ptr: %i)" u ptr

    let arg1 = readArg 1 posC
    let arg2 = readArg 2 posB
    let arg3 = readArg 3 posA

    let add ptr =
        (write (read (ptr + 3)) (arg1 ptr + arg2 ptr))
        (ptr + 4)

    let mult ptr =
        (write (read (ptr + 3)) (arg1 ptr * arg2 ptr))
        (ptr + 4)

    let input ptr =
        (write (read (ptr + 1)) (readInput ()))
        (ptr + 2)

    let output ptr =
        writeOutput (arg1 ptr)
        (ptr + 2)

    let jumpIfTrue ptr =
        if (arg1 ptr) <> 0 then (arg2 ptr) else (ptr + 3)

    let jumpIfFalse ptr =
        if (arg1 ptr) = 0 then (arg2 ptr) else ptr + 3

    let lessThan ptr =
        write
            (read (ptr + 3))
            (if (arg1 ptr) < (arg2 ptr) then 1 else 0)
        (ptr + 4)

    let equals ptr =
        write
            (read (ptr + 3))
            (if (arg1 ptr) = (arg2 ptr) then 1 else 0)
        (ptr + 4)

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
        | u -> failwithf "Unexpected opCode: %i (ptr: %i)" u ptr

    let runToOutput () =
        let rec run ptr =
            if not running then running
            elif wroteOutput then ptrOnPause <- ptr;  running
            elif halt ptr then running <- false; running
            else run ((operation ptr) ptr)
        wroteOutput <- false
        run ptrOnPause

    runToOutput

let amplify settings =
    let settings = Array.ofList settings
    let ampCount = settings.Length
    let ampCountDec = ampCount - 1
    let outputs = Array.init settings.Length (fun _ -> 0)

    let inputs setting input =
        let mutable cold = true
        fun () -> if cold then cold <- false; setting else input ()

    let amplifiers =
        settings
        |> Array.mapi (fun i setting ->
            let getInput = inputs setting (fun () ->
                outputs.[(i + ampCountDec) % ampCount])
            let setOutput value = outputs.[i] <- value
            computer program getInput setOutput)

    let rec amplify' () =
        amplifiers
            |> Array.map (fun amp -> amp ())
            |> Array.exists id // still running?
            |> function true -> amplify' () | _ -> outputs.[ampCountDec]
    amplify' ()

let findMaxPerm values =
    values
    |> permutations
    |> List.map amplify
    |> List.max

let Part1 () = findMaxPerm [0; 1; 2; 3; 4]

let Part2 () = findMaxPerm [5; 6; 7; 8; 9]
