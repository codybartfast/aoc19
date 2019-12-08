module Day07
let day = "07"

open System.IO

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj
let nPrint = id

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
let mutable program = compile lines.[0]

let computer readInput writeOutput =
    let memory = Array.copy program
    let reset ()  = program |> Array.iteri (fun i v -> memory.[i] <- v)

    let mutable wroteOutput = false
    let writeOutput v =
        wroteOutput <- true
        writeOutput v

    let read addr = memory.[addr]
    let write addr value = memory.[addr] <- value

    let halt ptr = read ptr = 99

    let posDE n = n % 100
    let posC n = n % 1000 / 100
    let posB n = n % 10000 / 1000
    let posA n = n % 100000 / 10000

    let opCode = read >> posDE

    let arg1 ptr =
        match (read >> posC) ptr with
        | 0 -> read (read (ptr + 1))
        | 1 -> read (ptr + 1)
        | u -> failwithf "Unexpected mode for first para: %i (ptr: %i)" u ptr

    let arg2 ptr =
        match (read >> posB) ptr with
        | 0 -> read (read (ptr + 2))
        | 1 -> read (ptr + 2)
        | u -> failwithf "Unexpected mode for second para: %i (ptr: %i)" u ptr

    let arg3 ptr =
        match (read >> posA) ptr with
        | 0 -> read (read (ptr + 3))
        | 1 -> read (ptr + 3)
        | u -> failwithf "Unexpected mode for thrid para: %i (ptr: %i)" u ptr

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

    let mutable ptrOnPause = 0
    let mutable running = true

    let step () =
        if running then
            let ptr = ptrOnPause
            if halt ptr 
            then running <- false
            else ptrOnPause <- ((operation ptr) ptr)
            running
        else
            running

    let runTillOutput () =
        wroteOutput <- false

        let rec execute ptr =
            if not running then
                running
            elif wroteOutput then
                ptrOnPause <- ptr
                running                
            elif halt ptr then
                running <- false; running
            else
                execute ((operation ptr) ptr)

        execute ptrOnPause

    let run () =
        while step () do ()
        false
        
    runTillOutput

let provideInput initValue source =
    let mutable unused = true
    fun () ->
        if unused
        then unused <- false; initValue
        else source ()

let run5 settings =
    let phaseSettings = Array.ofList settings

    let mutable outputA = 0
    let mutable outputB = 0
    let mutable outputC = 0
    let mutable outputD = 0
    let mutable outputE = 0

    let writeA value = outputA <- value

    let runA = 
        computer 
            (provideInput phaseSettings.[0] (fun () -> outputE))
            (fun value -> outputA <- value)
    let runB = 
        computer 
            (provideInput phaseSettings.[1] (fun () -> outputA))
            (fun value -> outputB <- value)
    let runC = 
        computer 
            (provideInput phaseSettings.[2] (fun () -> outputB))
            (fun value -> outputC <- value)
    let runD = 
        computer 
            (provideInput phaseSettings.[3] (fun () -> outputC))
            (fun value -> outputD <- value)
    let runE = 
        computer 
            (provideInput phaseSettings.[4] (fun () -> outputD))
            (fun value -> outputE <- value)

    let rec run () =
        printfn "%i - %i - %i - %i - %i" outputA outputB outputC outputD outputE    
        let areRunning = [runA (); runB (); runC (); runD (); runE ()]
        print areRunning
        print (List.exists id areRunning)
        if List.exists id areRunning then run ()
    run () 
    

    outputE
    // runA ()

let testA1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let testB1 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,"
                + "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

let Part1 () = ()
    // [0; 1; 2; 3; 4]
    // |> permutations
    // |> List.map run5
    // |> List.max

let Part2 () = 
    // program <- compile testA1
    // print (run5 [4; 3; 2; 1; 0])
    // program <- compile testB1
    // run5 [9; 8; 7; 6; 5]

    [5; 6; 7; 8; 9]
    |> permutations
    |> List.map run5
    |> List.max