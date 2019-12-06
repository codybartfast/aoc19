module Day06
let day = "06"

open System.IO

let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)

let parseLine (line: string) =
    let parts = line.Split ")"
    (parts.[0], parts.[1])

type Body =
    { Name: string;
      Axis: string;
      Orbits: int;
      mutable Sats: Body list }

let pairs = lines |> Array.map parseLine |> Set

let comMap =
    Map [("COM", { Name = "COM"; Axis = ""; Orbits = 0; Sats =[] })]

let addSat (bodies, pairs) =
    let (body, sat) =
        pairs
        |> Seq.pick (fun (body, sat) ->
            match Map.tryFind body bodies with
            | None -> None
            | Some body -> Some (body, sat))
    let sat =
        { Name = sat;
          Axis = body.Name;
          Orbits = body.Orbits + 1;
          Sats = [] }
    body.Sats <- sat::body.Sats
    (bodies.Add (sat.Name, sat))
    , (Set.remove (body.Name, sat.Name) pairs)

let rec buildBodies (bodies, pairs) =
    if Set.isEmpty pairs
    then bodies
    else addSat (bodies, pairs) |> buildBodies

let jumpCount a b bodies =
    let a = Map.find a bodies
    let b = Map.find b bodies

    let rec ancestors bodies body =
        if body.Name = "COM"
        then ["COM"]
        else body.Name::(ancestors bodies (Map.find body.Axis bodies))

    let aAncs = ancestors bodies a
    let bAncs = ancestors bodies b

    let rec orbitCount (aAncs: string list) count =
        match List.tryFindIndex ((=) aAncs.Head) bAncs with
        | Some c -> c + count
        | None -> orbitCount aAncs.Tail (count + 1)

    max 0 ((orbitCount aAncs 0) - 2)

let Part1 () =
    buildBodies (comMap, pairs)
    |> Map.toSeq
    |> Seq.sumBy (snd >> (fun b -> b.Orbits))

let Part2 () =
    jumpCount "YOU" "SAN" (buildBodies (comMap, pairs))
