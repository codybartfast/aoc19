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

type Body =
    {Name: string; Axis: string; Orbits: int; mutable Sats: Body list}

let pairs = lines |> Array.map parseLine |> Set
// let allLeft = pairs |> List.map fst |> Set
// let allRight = pairs |> List.map snd |> Set

let bodies =
    Map [ ("COM", { Name = "COM"; Axis = ""; Orbits = 0; Sats =[] }) ]

let addSat (bodies, pairs) =
    // let pairLen = Set.count pairs
    let (body, sat) =
        pairs
        |> Seq.pick (fun (body, sat) ->
            match Map.tryFind body bodies with
            | None -> None
            | Some body -> Some (body, sat))
    let sat =
        { Name = sat; Axis = body.Name; Orbits = body.Orbits + 1; Sats = []}
    body.Sats <- sat::body.Sats
    let bodies = bodies.Add (sat.Name, sat)
    let pairs = Set.remove (body.Name, sat.Name) pairs
    (bodies, pairs)
    // printfn "%i -> %i" pairLen (Map.count pairs)

let rec addSatelites (bodies, pairs) =
    if Set.isEmpty pairs
    then bodies
    else addSat (bodies, pairs) |> addSatelites

//let orbitCount a b =
let rec ancestors bodies body =
    if body.Name = "COM"
    then ["COM"]
    else body.Name::(ancestors bodies (Map.find body.Axis bodies))

let rec naiveOrbitCount a ancestors count =
    match ancestors with
    | [] -> None
    | h::_ when a = h -> Some count
    | _::t -> naiveOrbitCount a t (count + 1)

let orbitCount a b bodies =
    let aAncs = ancestors bodies a
    let bAncs = ancestors bodies b

    let rec orbitCount (aAncs: string list) count =
        match naiveOrbitCount aAncs.Head bAncs count with
        | Some count -> max 0 (count - 2)
        | None -> orbitCount aAncs.Tail (count + 1)

    orbitCount aAncs 0

let test = Regex.Split (@"COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN", @"\r?\n")

let Part1 () =
    addSatelites (bodies, pairs)
    |> Map.toSeq
    |> Seq.sumBy (snd >> (fun b -> b.Orbits))

let Part2 () =
    //let pairs = test |> Array.map parseLine |> Set
    let bodies = addSatelites (bodies, pairs)
    let santa = bodies.["SAN"]
    let me = bodies.["YOU"]

    orbitCount me santa bodies
