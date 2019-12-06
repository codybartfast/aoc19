module Day06
let day = "06"

open System
open System.IO
open System.Text.RegularExpressions

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let parseLine (line: string) =
    let parts = line.Split ")"
    (parts.[0], parts.[1])

type Body = {Name: string; Orbits: int; mutable Sats: Body list}

let pairs = lines |> Array.map parseLine |> Set
// let allLeft = pairs |> List.map fst |> Set
// let allRight = pairs |> List.map snd |> Set

let bodies = Map [ ("COM", { Name = "COM"; Orbits = 0; Sats =[] }) ]

let addSat (bodies, pairs) =
    // let pairLen = Set.count pairs
    let (body, sat) =
        pairs
        |> Seq.pick (fun (body, sat) -> 
            match Map.tryFind body bodies with
            | None -> None
            | Some body -> Some (body, sat))
    let sat = { Name = sat; Orbits = body.Orbits + 1; Sats = []}
    body.Sats <- sat::body.Sats
    let bodies = bodies.Add (sat.Name, sat)
    let pairs = Set.remove (body.Name, sat.Name) pairs
    (bodies, pairs)
    // printfn "%i -> %i" pairLen (Map.count pairs)

let rec addSatelites (bodies, pairs) =
    if Set.isEmpty pairs 
    then bodies
    else addSat (bodies, pairs) |> addSatelites


let Part1 () =
    addSatelites (bodies, pairs)
    |> Map.toSeq
    |> Seq.sumBy (snd >> (fun b -> b.Orbits))


let Part2 () =
    ()
