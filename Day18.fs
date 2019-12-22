module Day18
let day = "18"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%O" obj)
let tPrint obj = (print obj); obj

let toLines str = Regex.Split(str, @"\r?\n")

let test1a = toLines @"#########
#b.A.@.a#
#########"

let test1b = toLines @"########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################"

let test1c = toLines @"########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################"

let test1d = toLines @"#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################"

let test1e = toLines @"########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################"

let rec permutations list =
    // aoc15:13
    let rec insertAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insertAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insertAlong head) (permutations tail)

type Grid<'a when 'a : equality>(jagged: 'a[][]) =
    // aoc15:18
    let data = jagged
    let maxX = (Array.length (data.[0])) - 1
    let maxY = (Array.length data) - 1

    let mutable formatItem = (fun x -> x.ToString())

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member _.FormatItem with get() = formatItem and set(f) = formatItem <- f
    member this.AsText(x, y) = this.FormatItem (this.Item(x, y))

    member _.Row with get(y) = data.[y]
    member this.FormatRow = Array.map this.FormatItem >> (String.concat "")
    member this.AsText(y) = this.FormatRow (this.Row(y))

    member this.FormatGrid = Array.map this.FormatRow >> (String.concat nl)
    member this.AsText() = this.FormatGrid data
    override this.ToString() = this.AsText()
    member this.Display() = printfn "%s" (this.AsText())
    member this.TeeDisplay() = this.Display(); this

    member _.InBounds(x, y) = x >= 0 && x <= maxX && y >=0 && y <= maxY

    member this.Copy() = this.Transform (fun g x y -> g.[x,y])

    member this.Flatern() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield ((x, y), data.[y].[x]) }

    member _.Coords() =
        seq{ for y in 0 .. maxY do
                for x in 0 .. maxX do
                     yield (x, y) }

    member this.Filter(pred) = this.Flatern() |> Seq.filter (snd >> pred)

    member this.Find(pred) = this.Filter(pred) |> Seq.head |> fst

    member this.NHood(x, y) =
        [| for x in (x - 1)..(x + 1) do
            for y in (y - 1)..(y + 1) do
                if this.InBounds (x, y)
                then Some this.[x,y]
                else None |]

    member this.Adjacent(x, y) =
        let nhood = this.NHood (x, y)
        Array.append nhood.[0 .. 3] nhood.[5 .. 8]

    member this.Bordering(x, y) =
        [| this.TryGet (x, y - 1);
           this.TryGet (x + 1, y);
           this.TryGet (x, y + 1);
           this.TryGet (x - 1, y); |]

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. maxY do
                [| for x in 0 .. maxX do
                    generate this x y |] |]
            |> Grid<'b>

    member _.Corners() = [| (0, 0); (0, maxY); (maxX, maxY); (maxX, 0) |]
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
let parseLine (line: string) =
    Regex.Match(line, @"(.*)")
    |> fun (m: Match) ->
        let grp (idx: int) = m.Groups.[idx].Value
        let grpi = grp >> int
        grp 1

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

let overview triton start =
    explore triton start (Char.IsLetter) true
    |> Map

let paths overview keyCount =
    let paths = 
        overview
        |> Map.toSeq
        |> Seq.filter (fst >> isKey)
        |> Seq.map (fun (key, (path, _)) -> (key, path))
        |> Map
    assert (paths.Count = keyCount)
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

let furthestKey overview =
    overview
    |> Map.toList
    |> List.filter (fst >> isKey)
    // |> List.sortByDescending (snd >> snd)
    |> List.sortByDescending (snd >> fst >> List.length)
    |> List.head
    |> fst

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

let nextKey (keysHeld: Set<char>) reqdKeys =
    reqdKeys |> List.find (keysHeld.Contains >> not)

let findDist triton (overview: Map<char,list<char> * int>) =
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

let routeLength findDist route =
    route
    |> List.pairwise
    |> List.sumBy (fun (a, b) -> findDist a b)
    
let collect (triton: Triton) =

    let start = triton.Find ((=) '@')
    let overview = overview triton start
    let allKeys = triton.Filter isKey |> Seq.map snd |> Set
    let paths = paths overview (allKeys.Count)
    let tunnels = tunnels paths 
    let findDist = findDist triton overview
    let furthestKey = furthestKey overview
    

    let rec collect (dist: int) keysHeld here = seq{
        if keysHeld = allKeys then yield dist else

        let reachable = reachable tunnels allKeys keysHeld
        let unreachableReqdKeys = 
            reqdKeys paths furthestKey
            |> List.skipWhile (fun k -> 
                reachable.Contains k || keysHeld.Contains k)
        let routes =
            match unreachableReqdKeys with
            | [] -> 
                permutations (Set.toList reachable)
                |> List.map (fun route ->  here::route)
            | h::_ -> 
                permutations (Set.toList reachable)
                |> List.map (fun route -> here::(List.append route [h]))
        let route = routes |> List.minBy (routeLength findDist)
        let next = route |> List.item 1

        // printfn  " -> %A" next
        // printfn "h: %A; r: %A; u:%A" keysHeld reachable unreachableReqdKeys

        yield! collect
                (dist + (findDist here next))
                (keysHeld.Add next)
                next}
           
    collect 0 Set.empty '@'


let Part1 () = ()

    
let Part2 () =
    let triton = buildTriton input
    collect triton


