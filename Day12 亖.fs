module Day12
let day = "12"

open System.IO
open System.Text.RegularExpressions

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
let simulation = Seq.unfold (fun moons ->
    let newMoons = stepAll moons
    Some (moons, newMoons))

let Part1 () =
    moons input
    |> simulation
    |> Seq.item 1000
    |> totalEnergy

let xInfo {Pos = (x, _, _); Vel = (a, _, _)} = (x, a)
let yInfo {Pos = (_, y, _); Vel = (_, b, _)} = (y, b)
let zInfo {Pos = (_, _, z); Vel = (_, _, c)} = (z, c)

let period dimInfo =
    let scan =
        Seq.scan
            (fun (set, repeat) value ->
                if Set.contains value set
                then (set, Some (set.Count, value))
                else ((Set.add value set), None) )
            (Set.empty, None)
        >> Seq.pick snd

    Seq.map (List.map dimInfo)
    >> scan
    >> fst >> int64

let rec lcm x y =
    let rec hcf a b = if b = 0L then abs a else hcf b (a % b)
    x * (y / (hcf x y))

let Part2 () =
    let sim = input |> moons |> simulation |> Seq.cache
    [period xInfo sim; period yInfo sim; period zInfo sim]
    |> List.reduce lcm
