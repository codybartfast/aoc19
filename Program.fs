
open System.IO
open System.Diagnostics

printfn ""
let sw = Stopwatch ()
let showTime () = 
    printfn "(%s seconds)" (sw.Elapsed.TotalSeconds.ToString("n3"))
    printfn ""

let getInputFile name =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", "input" + name + ".txt") 
let readLines name = File.ReadAllLines(getInputFile name)

open Day01
let name = "01"

let lines = readLines name

[<EntryPoint>]
let main argv =

    let input1 =  lines  

    printfn "Part 1 ..."
    sw.Restart ()
    let result1 = Part1 input1
    sw.Stop ()
    printfn "%A" result1
    showTime ()

    printfn "Part 2 ..."
    sw.Restart() 
    let result2 = Part2 result1 input1
    printfn "%A" result2
    showTime ()

    0
