open Day25 // file needs to be included in AdventOfCode2019.fsproj

let time title fn =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start ()
    let result = fn ()
    sw.Stop ()
    let elapsed = sw.Elapsed.TotalSeconds.ToString("n3")
    printfn "%s (%ss): %A" title elapsed result

[<EntryPoint>]
let main _ =
    time "Part 1" Part1
    time "Part 2" Part2
    0
