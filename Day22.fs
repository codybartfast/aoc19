module Day22
let day = "22"

#nowarn "0025"

open System.IO
open System.Text.RegularExpressions

type Deal =
    | Increment of bigint
    | Cut of bigint
    | Stack

let readLines day =
    File.ReadAllLines (Path.Combine("inputs", "input" + day + ".txt"))
let lines = readLines day

let parseLine (line: string) =
    if line.Contains("stack") then
        Stack
    else
        let num = bigint.Parse(Regex.Match(line, @"-?\d+").Value)
        if line.StartsWith("deal")
        then Increment num
        else Cut num

let apply deal =
    match deal with
    | Stack -> Array.rev
    | Cut n -> (fun arr ->
        let n = int n
        let n = if n >= 0 then n else (arr.Length + n )
        Array.append arr.[n ..] arr.[0 .. (n - 1)])
    | Increment n -> (fun arr ->
        let n = int n
        let len = arr.Length
        let arr' = Array.zeroCreate len
        [0 .. (len - 1)]
        |> List.iter (fun i ->
            arr'.[(i * n) % len] <- arr.[i])
        arr' )

let shuffle deck deals =
    (deck, deals)
    ||> Array.fold (fun deck deal ->
        ((apply deal) deck))

let simplify (deckSize: bigint) deals =
    let simplifyStack (len: bigint) deals =
        let rec simp rslt deals reversed =
            match deals, reversed with
            | [], false -> rslt
            | [], true -> Stack::rslt
            | Stack::rest, _ -> simp rslt rest (not reversed)
            | deal::rest, false -> simp (deal::rslt) rest reversed
            | Cut n :: rest, _ -> simp ((Cut -n)::rslt) rest reversed
            | Increment n :: rest, _ ->
                simp
                    (Cut n::Increment (deckSize - n)::rslt)
                    rest
                    (not reversed)
        simp [] deals false
        |> List.rev

    let simplifyRest (len: bigint) deals =
        let rec simp deals =
            match deals with
            | [] -> []
            | [a] -> [a]
            | Cut m :: Cut n :: rest ->
                simp (Cut ((m + n) % len):: rest)
            | Increment m :: Increment n :: rest ->
                simp (Increment ((m * n) % len) :: rest)
            | a::[b] -> a::[b]
            | Increment _ :: Cut _ :: [Stack] as simplified -> simplified
            | Cut m :: Increment n :: rest ->
                simp (Increment n :: Cut ((m * n) % len) :: rest)
            | Increment l :: Cut m :: Cut n :: rest ->
                simp (Increment l :: Cut ((m + n) % len) :: rest)
            | Increment l :: Cut m :: Increment n :: rest ->
                simp (
                    Increment l ::
                    Increment n ::
                    Cut ((m * n) % len) :: rest)
        simp deals

    deals
    |> List.ofArray
    |> simplifyStack deckSize
    |> simplifyRest deckSize
    |> Array.ofList

let repeat deals reps =
    [|1 .. reps|]
    |> Array.collect (fun _ -> deals)

let repeatAndSimplify deckSize deals reps =
    repeat deals reps
    |> simplify deckSize

let bigRepeatAndSimplify deckSize deals reps =
    let rec bras accumDeals baseDeals reps =
        if reps = 0L then accumDeals else
        let quotient, remainder = reps / 10L, int (reps % 10L)
        let sally = repeatAndSimplify deckSize baseDeals remainder
        let acDeals = simplify deckSize (Array.append sally accumDeals)
        let bsDeals = repeatAndSimplify deckSize baseDeals 10
        bras acDeals bsDeals quotient
    bras [||] deals reps

let applyCut deckSize cut pos = ((pos + deckSize) - cut) % deckSize
let reverseCut deckSize cut pos = ((pos + deckSize) + cut) % deckSize

let applyIncs deckSize inc pos = (inc * pos) % deckSize
let reverseIncs deckSize inc pos =
    let rec increments deckSize increment steps =
        seq{ yield (increment, steps)
             if increment > bigint.One then
                let increment' = (increment - (deckSize % increment))
                let steps' = ((deckSize / increment) + bigint.One) * steps
                yield! increments deckSize increment' steps'}

    let rec decon increments total = seq{
        if total = bigint.Zero then () else
        let h::t = increments
        if (fst h) <= total then
            yield h
            yield! decon increments (total - (fst h))
        else
            yield! decon t total }

    let increments = increments deckSize inc bigint.One |> List.ofSeq
    let decon = decon increments (pos) |> List.ofSeq
    let prePos = (decon |> Seq.sumBy snd) % deckSize
    prePos

let Part1 () =
    let deck = [|0 .. 10_006|]
    let deals = lines |> Array.map parseLine
    (shuffle deck deals)
    |> Array.findIndex ((=) 2019)

let Part2 () =
    let deals = Array.map parseLine lines
    let deckSize = bigint 119315717514047L
    let reps = 101741582076661L
    let finalPos = bigint 2020L
    let bigDeals = bigRepeatAndSimplify deckSize deals reps
    let [|Increment inc; Cut cut; |] = bigDeals
    let preCutPos = reverseCut deckSize cut finalPos
    let preIncPos = reverseIncs deckSize inc preCutPos
    preIncPos
