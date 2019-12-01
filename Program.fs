
open System.IO
open System.Diagnostics

let sw = Stopwatch ()

let showTime () =
    printfn "(%s seconds)" (sw.Elapsed.TotalSeconds.ToString("n3"))
    printfn ""

let inputFile name =
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", "input" + name + ".txt")


// To change day make sure the day's file is included in AdventOfCode2019.fsproj
// and change this line.  Dates are zero padded, i.e. Day01 not Day1.
open Day02


[<EntryPoint>]
let main argv =

    let input1 = File.ReadAllLines(inputFile day)

    printfn ""
    printfn "Part 1 ..."
    sw.Restart ()
    let result1 = Part1 input1
    sw.Stop ()
    printfn "%A" result1
    showTime ()

    printfn "Part 2 ..."
    sw.Restart()
    let result2 = Part2 result1 input1
    sw.Stop()
    printfn "%A" result2
    showTime ()

    0
