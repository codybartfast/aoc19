open Day03

open System.Diagnostics

let sw = Stopwatch ()
let time () = sw.Elapsed.TotalSeconds.ToString("n3")

[<EntryPoint>]
let main _ =
    printfn "Line count: %i" (lines.Length)

    sw.Restart ()
    let result1 = Part1 ()
    sw.Stop ()
    printfn "Part 1 (%ss): %A" (time ())  result1

    sw.Restart()
    let result2 = Part2 ()
    sw.Stop()
    printfn "Part 2 (%ss): %A" (time ())  result2

    0
