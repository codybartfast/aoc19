module Day18
let day = "18"

open System
open System.IO

// Nothing good can be said about this solution except it worked, even if it
// shouldn't have.

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    member _.LastCol = maxX
    member _.Width = maxX + 1
    member _.LastRow = maxY
    member _.Height = maxY + 1

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Flatten() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member this.Filter(pred) = this.Flatten() |> Seq.filter (snd >> pred)

    member this.Find(pred) = this.Filter(pred) |> Seq.head |> fst

    member this.NHoodCoords(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do (x, y) |]

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

    member _.Crop(x, width, y, height) =
        data.[y .. (y + height - 1)]
        |> Array.map (fun row -> row.[x .. (x + width - 1)])
        |> Grid<'a>

    member this.Get((x, y)) = this.[x, y]
    member this.Set((x, y)) value = this.[x, y] <- value
    member this.TryGet((x, y)) =
        match this.InBounds(x, y) with
        | true -> Some (this.Get((x, y)))
        | false -> None

type Triton = Grid<char>

let tritonGrid =
    Array.map (fun (s: string) -> s.ToCharArray())
    >> Triton

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let input = readLines day
let buildTriton lines = tritonGrid (lines)

type Location = Wall | Clear | Origin | Oxygen
type Direction = North | South | West | East

let isDoor = Char.IsUpper
let isKey = Char.IsLower
let keyForDoor = Char.ToLower

let explore (triton: Triton) start isTarget contOnFind =
    let nextPosition (x, y) dir =
        match dir with
        | North -> (x, y - 1)
        | South -> (x, y + 1)
        | West -> (x - 1, y)
        | East -> (x + 1, y)

    let right = function
        | North -> East | South -> West | West -> North | East -> South
    let left = function
        | North -> West | South -> East | West -> South | East -> North
    let reverse = function
        | North -> South | South -> North | West -> East | East -> West

    let rec explore been hist pos dist =
        let been = Set.add pos been
        [North; South; West; East]
        |> Seq.map (nextPosition pos)
        |> Seq.filter (been.Contains >> not)
        |> Seq.collect (fun next -> seq{
            match triton.Get next with
            | '#' -> ()
            | key when (isTarget key) ->
                yield Some (key, (List.rev (key::hist), dist))
                if contOnFind then
                    yield! explore been (key::hist) next (dist + 1)
            | door when isDoor door ->
                yield! explore been (door::hist) next (dist + 1)
            | _ -> yield! explore been hist next (dist + 1) })

    explore Set.empty [] start 1
    |> Seq.choose id
    |> Seq.groupBy fst
    |> Seq.map (fun (_, keys) -> keys |> Seq.minBy snd)
    |> List.ofSeq

let overview (triton: Triton) start =
    explore triton start (Char.IsLetter) true
    |> Map

let paths overview =
    let paths =
        overview
        |> Map.toSeq
        |> Seq.filter (fst >> isKey)
        |> Seq.map (fun (key, (path, _)) -> (key, path))
        |> Map
    paths

let rec startsWith list1 list2 =
    match list1, list2 with
    | _, [] -> true
    | [], _ -> false
    | h1::t1, h2::t2 when h1 = h2 -> startsWith t1 t2
    | _ -> false

let tunnels paths =
    let paths =
        paths
        |> Map.toList
        |> List.map snd
        |> List.sortByDescending id
    let rec tunnels keep (paths: char list list) =
        match paths with
        | [] -> keep
        | [a] -> a::keep
        | a::b::c when startsWith a b -> tunnels keep (a::c)
        | a::b::c -> tunnels (a::keep) (b::c)
    tunnels [] paths
    |> List.rev

let reqdKeys paths key =
    let deDup items =
        let rec deDup seen deDupped items =
            match items with
            | [] -> List.rev deDupped
            | h::t ->
                if Set.contains h seen
                then deDup seen deDupped t
                else deDup (seen.Add h) (h::deDupped) t
        deDup Set.empty [] items

    let rec reqdKeys paths key = seq{
        yield key
        yield!
            Map.find key paths
            |> List.filter isDoor
            |> Seq.collect (keyForDoor >> (reqdKeys paths)) }

    reqdKeys paths key
    |> List.ofSeq
    |> List.rev
    |> deDup

let reachable tunnels allKeys keysHeld =
    let wanted = Set.difference allKeys keysHeld
    let reachable =
        tunnels
            |> List.collect (List.takeWhile (fun chr ->
                isKey chr || keysHeld.Contains (keyForDoor chr)))
            |> Set
    Set.intersect wanted reachable

let places (triton: Triton) =
    triton.Filter (fun x -> x <> '.' && x <> '#')
    |> Seq.map (fun (x, y) -> y, x)
    |> Map

let keys places =
    places
    |> Map.toList
    |> List.map fst
    |> List.filter isKey
    |> List.sort

let doors places =
    places
    |> Map.toList
    |> List.map fst
    |> List.filter isDoor
    |> List.sort

let distance (triton: Triton) =
    let places = places triton
    let overview = overview triton places.['@']
    let keys = keys places

    let calcDist =
        let heads =
            overview
            |> Map.toList
            |> List.map (snd >> fst >> List.head)
            |> Set

        let startToHeads =
            heads
            |> Seq.map (fun h ->
                let _, d = overview.[h]
                (('@', h), d))

        let headsToHeads =
            let headToHeads (loc, head) =
                explore triton loc (heads.Contains) false
                |> List.map (fun (other, (_, d)) -> ((head, other), d))
            let coords = triton.Filter (heads.Contains)
            coords
            |> Seq.collect headToHeads

        let known =
            Seq.append startToHeads headsToHeads
            |> Seq.fold (fun m (k, v) -> Map.add k v m) Map.empty

        let head a = overview.[a] |> (fst >> List.head)
        let dist a = overview.[a] |> snd
        let depth a = (dist a) - (dist (head a))
        let path a = overview.[a] |> fst

        let diff a b =
            if startsWith (path a) (path b)
            then depth a - depth b
            elif startsWith (path b) (path a)
            then depth b - depth a
            else
                let loc = triton.Find ((=) a)
                explore triton loc ((=) b) false
                    |> List.head |> snd |> snd

        fun a b ->
            if a = b then 0
            elif a = '@' then
                known.[(a, head b)] + depth b
            else
                let aHead, bHead =  head a, head b
                if aHead <> bHead
                then known.[(aHead, bHead)] + depth a + depth b
                else diff a b

    let distTable =
        let placesOfInterest = '@'::keys
        [for a in placesOfInterest do for b in placesOfInterest do (a, b)]
        |> List.filter (fun (a, b) -> a < b)
        |> List.map (fun (a, b) -> (a, b), calcDist a b)
        |> Map

    fun a b ->
        if a = b then 0
        elif a < b then Map.find (a,b) distTable
        else distTable.[b, a]

let routeLength (distance: 'a -> 'a -> int) (route: List<'a>) =
    route
    |> List.pairwise
    |> List.sumBy (fun (a, b) -> distance a b)

let collect findDist paths terminus =
    let tunnels = tunnels paths
    let targetKeys = Set <| reqdKeys paths terminus

    let rec collect (dist: int) keysHeld here = seq{
        let keysHeldSet = Set keysHeld
        if keysHeldSet = targetKeys then yield (dist, keysHeld) else

        let reachable = reachable tunnels targetKeys keysHeldSet
        yield!
            reachable
            |> Seq.collect (fun key ->
                collect
                    (dist + findDist here key)
                    (key::keysHeld)
                    key) }

    collect 0 [] '@'
    |> Seq.minBy fst
    |> snd
    |> List.rev
    |> fun lst -> '@'::lst

let merge findDist route1 route2 =
    let routeLen = routeLength findDist
    let isCmn = (Set.intersect (Set route1) (Set route2)).Contains
    let section route =
        let section = route |> List.takeWhile (isCmn >> not)
        section, (route |> List.skip section.Length)

    let mergeSection inner outer =
        let rec merge rslt inner outer = seq{
            match inner, outer with
            | [], [] -> yield rslt
            | [], h::t -> yield! merge (h::rslt) inner t
            | _::_, [] -> failwith "didn't expect this"
            | h::t, [a] ->  yield! merge (h::rslt) t [a]
            | h1::t1, h2::t2 ->
                yield! merge (h1::rslt) t1 (h2::t2)
                yield! merge (h2::rslt) (h1::t1) t2 }
        merge [] inner (List.tail outer)
        |> Seq.minBy (fun lst -> routeLen (outer.Head::lst))

    let rec merge rslt route1 route2 =
        match route1, route2 with
        | [], [] -> rslt
        | h::tail, [] -> merge (h::rslt) tail route2
        | [], h::tail -> merge (h::rslt) route1 tail
        | h1::tail1, h2::tail2 when isCmn h1 && isCmn h2 ->
            assert (h1 = h2)
            merge (h1::rslt) tail1 tail2
        | h1::_, h2::tail2 when isCmn h1 ->
            merge (h2::rslt) route1 tail2
        | h1::tail1, h2::_ when isCmn h2 ->
            merge (h1::rslt) tail1 route2
        | _ ->
            let prev = rslt.Head
            match section route1, section route2 with
            | (unc1, h1::t1), (unc2, h2::t2) ->
                let mergedSection =
                    mergeSection
                        unc1
                        (prev::(List.append unc2 [h2]))
                merge
                    (List.append mergedSection.Tail rslt)
                    (h1::t1)
                    (h2::t2)
            | (unc1, []), (unc2, []) ->
                let ms1 = mergeSection unc1 (prev::unc2)
                let ms2 = mergeSection unc2 (prev::unc1)
                let mergedSection =
                    if routeLen ms1 <= routeLen ms2 then ms1 else ms2
                List.append mergedSection rslt
            | _ -> failwith "didn't expect this"

    List.rev <| merge [] route1 route2

let part1Solve (triton: Triton) =
    let places = places triton
    let overview = overview triton places.['@']
    let paths = paths overview
    let termini = paths |> tunnels |> List.map List.last

    let distance = distance triton

    let routes =
        termini
        |> List.map (fun terminus ->
            collect distance paths terminus)
        |> List.sortByDescending List.length

    let routeLength = routeLength distance
    routes
    |> List.reduce (merge distance)
    |> routeLength

let patch (triton: Triton) =
    let start = (triton.Width / 2, triton.Height / 2)
    let coords = (triton.NHoodCoords start)
    let values = [| '@'; '#'; '@'; '#'; '#'; '#'; '@'; '#'; '@'; |]
    Array.iter2 triton.Set coords values

let quads (triton: Triton) =
    let width = triton.Width
    let height = triton.Height
    (assert (width % 2 <> 0))
    (assert (height % 2 <> 0))
    let haWidth, haHeight = width / 2, height / 2
    let haWidthPlus, haHeightPlus  = haWidth + 1, haHeight + 1
    [ triton.Crop (0, haWidthPlus, 0, haHeightPlus);
      triton.Crop (haWidth, haWidthPlus, 0, haHeightPlus);
      triton.Crop (haWidth, haWidthPlus, haHeight, haHeightPlus)
      triton.Crop (0, haWidthPlus, haHeight, haHeightPlus); ]

let openRemoteDoors triton =
    let places = places triton
    let doors = doors places
    let keys = keys places
    Set.difference (Set doors) (Set keys)
    |> Seq.iter (fun door ->
        let coord = triton.Find ((=)door)
        triton.Set coord '.' )

let thorough (triton: Triton) =
    let start = triton.Find ((=) '@')
    let overview = overview triton start
    let paths = paths overview
    let tunnels = tunnels paths
    let findDist = distance triton
    let targetKeys = triton.Filter isKey |> Seq.map snd |> Set

    let rec collect (dist: int) keysHeld here = seq{
        let keysHeldSet = Set keysHeld
        if keysHeldSet = targetKeys then yield (dist, keysHeld) else

        let reachable = reachable tunnels targetKeys keysHeldSet
        yield!
            reachable
            |> Seq.collect (fun key ->
                collect
                    (dist + findDist here key)
                    (key::keysHeld)
                    key) }

    collect 0 [] '@'
    |> Seq.minBy fst
    |> snd
    |> List.rev
    |> fun lst -> '@'::lst
    |> routeLength findDist

let Part1 () =
    part1Solve (buildTriton input)

let Part2 () =
    let triton = buildTriton input
    patch triton
    openRemoteDoors triton
    let [nw; ne; se; sw] = (quads triton)
    (List.sumBy thorough [nw; ne; se])
        + part1Solve sw
