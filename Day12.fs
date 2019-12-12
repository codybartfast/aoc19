module Day12
let day = "12"

open System
open System.IO
open System.Text.RegularExpressions

let nl = System.Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parseLine (line: string) =
    Regex.Match(line, @"<x=(.*), y=(.*), z=(.*)>")
    |> fun (m: Match) ->
        let grp (idx: int) = m.Groups.[idx].Value
        let grpi = grp >> int
        grpi 1, grpi 2, grpi 3
let input = lines

type Moon = { Pos: int * int * int; Vel: int * int * int }
let moon pos = { Pos = pos; Vel = (0, 0, 0)}
let moons =  Array.map parseLine >> Array.map moon >> List.ofArray

let add (x, y, z) (x', y', z') = (x + x', y + y', z + z')

let totalEnergy =
    List.sumBy (fun moon ->
        let {Pos = (x, y, z); Vel = (a, b, c)} = moon
        (abs x + abs y + abs z) * (abs a + abs b + abs c))

let updateVel moons moon =
    let deltaVel =
        let deltaVel1 moon moon'=
            let deltaDim m n =
                match n - m with
                | 0 -> 0
                | del -> del / abs del
            let {Pos = (x, y, z); Vel = (a, b, c)} = moon
            let {Pos = (x', y', z'); Vel = (a', b', c')} = moon'
            (deltaDim x x', deltaDim y y', deltaDim z z')
        moons
        |> List.map (deltaVel1 moon)
        |> List.reduce add
    {moon with Vel = add moon.Vel deltaVel}
let move moon = {moon with Pos = add moon.Pos moon.Vel}
let step moons =List.map ((updateVel moons) >> move)
let stepAll moons = moons |> (step moons)
let rec steps = Seq.unfold (fun moons ->
    let newMoons = stepAll moons
    Some (moons, newMoons))

let test1a = Regex.Split (@"<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>", @"\r?\n")

let test1b = Regex.Split (@"<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>", @"\r?\n")

let Part1 () =
    moons input
    |> steps
    |> Seq.item 999
    |> totalEnergy

let scan things =
    things
    |> Seq.scan
        (fun (set, repeat) value ->
            if Set.contains value set
            then ((Set.add value set), Some value)
            else ((Set.add value set), None) )
        (Set.empty, None)


let mapX moons =
    let map {Pos = (x, _, _); Vel = (a, _, _)} =
        (x, a)
    moons
    |> List.map map

let mapY moons =
    let map {Pos = (_, y, _); Vel = (_, b, _)} =
        (y, b)
    moons
    |> List.map map

let mapZ moons =
    let map {Pos = (_, _, z); Vel = (_, _, c)} =
        (z, c)
    moons
    |> List.map map

let firstRep steps map =
    steps
    |> Seq.map map
    |> scan
    |> Seq.mapi (fun i (s, v) -> i, v)
    |> Seq.filter (fun (i, v) -> v <> None)
    |> Seq.head
    |> fst
    |> (fun n -> n - 1)
    |> int64

let rec lcm x y =
    let rec hcf a b = if b = 0L then abs a else hcf b (a % b)
    x * y / (hcf x y)

let Part2 () =
    let data = input
    let steps = steps (moons data) |> Seq.cache
    let fr = firstRep steps
    let (xr, yr, zr) = tPrint (fr mapX, fr mapY, fr mapZ)
    let lcmXrYr = lcm xr yr
    let lcm = lcm lcmXrYr zr
    lcm
