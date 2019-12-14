module Day14
let day = "14"

open System.IO

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day
let input = lines

let parseLine (line: string) =
    let parts = line.Split " => "
    let (inputs, (qty, output)) =
        let qtyChem (qtyChem: string) =
            let parts = qtyChem.Split " "
            (int64 parts.[0], parts.[1])
        let inputs =
            parts.[0].Split ", " |> Array.map qtyChem |> List.ofArray
        (inputs, qtyChem parts.[1])
    output, (qty, inputs)

let reactions = Array.map parseLine >> Map

let addInventory inventory (quantity, output) =
    let existingQty =
        match Map.tryFind output inventory with
        | Some qty -> qty | None -> 0L
    inventory.Add (output, existingQty + quantity)

let rec takeIventory reactions inventory (qty, output) =
    let manufacture (qty, output) =
        let (reactQty, reactInputs) = Map.find output reactions
        let reactCount =  ((qty - 1L) / reactQty) + 1L
        let batchInputs =
            reactInputs |> List.map (fun (qty, a) -> (qty * reactCount, a))
        let inventory =
            (inventory, batchInputs) ||> List.fold (takeIventory reactions)
        addInventory inventory (reactCount * reactQty, output)

    let existingQty =
        match Map.tryFind output inventory with
        | Some qty -> qty | None -> 0L
    if output = "ORE" || qty <= existingQty then
        // reduce inventory
        inventory.Add (output, existingQty - qty)
    else
        let inventory = manufacture (qty - existingQty, output)
        takeIventory reactions inventory (qty, output)

let oreUsed fuel =
    takeIventory (reactions input) Map.empty (fuel, "FUEL")
    |> Map.find  "ORE"
    |> ((*) -1L)

let binarySearch tooFar =
    let rec search lower upper =
        if not (tooFar upper) then
            search lower (2L * upper)
        elif lower + 1L = upper then
            lower
        else
            let mid = (lower + upper) / 2L
            if tooFar mid then search lower mid else search mid upper
    search 0L 1L

let Part1 () = oreUsed 1L

let Part2 () = binarySearch (oreUsed >> ((<) 1_000_000_000_000L))
