open Day04 // file needs to included in AdventOfCode2019.fsproj

let sw = System.Diagnostics.Stopwatch ()

let time title fn = 
    sw.Restart ()
    let result = fn ()
    sw.Stop ()
    let elapsed = sw.Elapsed.TotalSeconds.ToString("n3")
    printfn "%s (%ss): %A" title elapsed result

[<EntryPoint>]
let main _ =
    printfn "Line count: %i" (lines.Length)
    time "Part 1" Part1
    time "Part 2" Part2
    0
