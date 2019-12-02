module Day02
let day = "02"

#nowarn "0025"


(* ================ Part 1 ================ *)


let run (mem: int[]) (noun, verb) =  
    let rec execute ptr =
        if mem.[ptr] = 99 then
            (mem.[0], 100 * noun + verb)
        else
            let [| code; src1; src2; dest |] = mem.[ptr .. ptr + 3]
            let operation = match code with 1 -> (+) | 2 -> (*)
            mem.[dest] <- operation mem.[src1] mem.[src2]
            execute (ptr + 4)
    
    mem.[1] <- noun
    mem.[2] <- verb
    execute 0

let Part1 (lines : string[]) =
    let input = lines.[0].Split "," |> Array.map int
    run input (12, 02) |> fst


(* ================ Part 2 ================ *)


let Part2 r1 (lines : string[]) =
    let target = 19690720
    let input = lines.[0].Split "," |> Array.map int
    let memory = Array.copy input
    let reset ()  = input |> Array.iteri (fun i v -> memory.[i] <- v)

    [ for noun in 0..99 -> [for verb in 0..99  -> (noun, verb) ]]
    |> Seq.collect id
    |> Seq.map (fun nv ->
        reset ()
        run memory nv)
    |> Seq.find (fst >> (=) target)
    |> snd
