module Day14
let day = "14"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let testLines str = Regex.Split(str, @"\r?\n")

let test1a = testLines @"10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"

let test1b = testLines @"9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"

let test1e = testLines @"171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX"

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day
let input = lines

let parseLine (line: string) =
    let quanity (agent: string) =
        let parts = agent.Split " "
        (parts.[1], int parts.[0])
    let parts = line.Split " => "
    let (agents, (product, qty)) =
        ( parts.[0].Split ", " |> Array.map quanity |> List.ofArray,
            parts.[1] |> quanity)
    product, (qty, agents)

let reactions = Array.map parseLine >> Map

let addInventory inventory (product, quantity) =
    let existingQty =
        match Map.tryFind product inventory with
        | Some qty -> qty | None -> 0
    inventory.Add (product, existingQty + quantity)

let rec takeIventory reactions inventory (product, requiredQty) =
    let existingQty =
        match Map.tryFind product inventory with
        | Some qty -> qty | None -> 0
    if product = "ORE" || requiredQty <= existingQty
    then inventory.Add (product, existingQty - requiredQty)
    else
        let manRequired = requiredQty - existingQty
        let (procQty, procAgents) = Map.find product reactions
        let batchs =  ((manRequired - 1) / procQty) + 1
        let batchAgents =
            procAgents |> List.map (fun (a, qty) -> (a, qty * batchs))
        let invLessAgents =
            List.fold (takeIventory reactions) inventory batchAgents
        let produced = batchs * procQty
        let invWithProduced = addInventory invLessAgents (product, produced)
        takeIventory reactions invWithProduced (product, requiredQty)

let Part1 () =
    let finalInv = takeIventory (reactions input) Map.empty ("FUEL", 1)
    finalInv.["ORE"] |> ((*) -1)

let Part2 () =
    ()
