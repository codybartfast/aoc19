module Day22
let day = "22"

open System
open System.IO
open System.Text.RegularExpressions

let nl = Environment.NewLine
let print obj= (printfn "%A" obj)
let tPrint obj = (print obj); obj

let toLines (text: string) = Regex.Split(text.Trim(), @"\r?\n")

let test1a = toLines @"deal with increment 7
deal into new stack
deal into new stack"

let test1b = toLines @"cut 6
deal with increment 7
deal into new stack"

let test1c = toLines @"deal with increment 7
deal with increment 9
cut -2"

let test1d = toLines @"
deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1
"

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
                // let m = if m >= bigint.Zero then m else (len + m)
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

let slowCheck deckSize deals =
    let deck () = [| 0 .. ((int deckSize) - 1)|]
    let compare deals1 deals2 =
        let rslt1 = shuffle (deck ()) deals1
        let rslt2 = shuffle (deck ()) deals2
        assert (rslt1 = rslt2)

    let simplified = simplify deckSize deals
    compare deals simplified

    let big = repeat deals (int (deckSize - bigint.One))
    let simple = simplify deckSize big
    let combo = repeat simplified (int (deckSize - bigint.One))
    let sCombo = simplify deckSize combo
    compare big simple
    compare simple combo
    compare combo sCombo


let Part1 () =
    let deck = [|0 .. 10_006|]
    let deals = lines |> Array.map parseLine

    (shuffle deck deals)
    |> Array.findIndex ((=) 2019)

let bigRepeastAndSimplify deckSize deals reps =
    let rec bras accumDeals baseDeals reps =
        if reps = 0L then accumDeals else
        let quotient, remainder = reps / 10L, int (reps % 10L)
        let sally = repeatAndSimplify deckSize baseDeals remainder
        let acDeals = simplify deckSize (Array.append sally accumDeals)
        let bsDeals = repeatAndSimplify deckSize baseDeals 10
        bras acDeals bsDeals quotient
    bras [||] deals reps

let bigCheck deckSize deals =
    let expected = repeatAndSimplify deckSize deals   579
    let actual = bigRepeastAndSimplify deckSize deals 579L
    match expected.[0], actual.[0] with
    | Increment m, Increment n -> 
        printfn "BigCheck: %A %A" (m + n) ((m + n) % deckSize)
    assert (expected = actual) 

// let applyStack deckSize pos = (deckSize - bigint.One) - pos
let applyCut deckSize cut pos = ((pos + deckSize) - cut) % deckSize
let reverseCut deckSize cut pos = ((pos + deckSize) + cut) % deckSize

let rec increments (deckSize: bigint) (increment: bigint) (steps: bigint) =
    seq{ yield (increment, steps)
         if increment > bigint.One then
            let increment' = (increment - (deckSize % increment))
            let steps' = ((deckSize / increment) + bigint.One) * steps
            yield! increments deckSize increment' steps'}

let rec decon increments total = seq{
    if total = bigint.One then () else
    let h::t = increments
    if (fst h) <= total then
        yield h
        yield! decon increments (total - (fst h))
    else
        yield! decon t total }

let applyIncs (deckSize: bigint) (inc: bigint) (count: bigint) =
    (inc * count) % deckSize

let Part2 () =
    let deals = Array.map parseLine lines

//     let parse = Array.map parseLine
//     let deals = parse scrap1
//     let deckSize1 = 10_007L
//     // let deckSize1 = 107L
//     let deck1 = [| 0 .. ((int deckSize1) - 1) |]
//     let inc = 10_002
//
//     let scrap1 = toLines (sprintf "
// deal into new stack
// deal with increment %i
// " inc)
//
//     let scrap2 = toLines (sprintf "
// deal with increment %i
// cut %i
// " (deckSize1 - (int64 inc)) inc)
//
//     print (shuffle deck1 (parse scrap1))
//     print (shuffle deck1 (parse scrap2))
//     print (Array.findIndex ((=) ((int deckSize1) - 1)) (shuffle deck1 (parse scrap2)))

    let deckSize1 = bigint 10_007L
    let deck1 = [| 0 .. ((int deckSize1) - 1) |]
    // slowCheck deckSize1 deals
    bigCheck deckSize1 deals
    let baseDeals1 = simplify deckSize1 deals
    // Part 1 same with simpified rules:
    assert (1252 = (shuffle deck1 baseDeals1 |> Array.findIndex ((=) 2019)))

    let deckSize = bigint 119315717514047L

    let set = simplify deckSize deals

    bigCheck deckSize (simplify deckSize set)
    let reps = 101741582076661L
    let finalPos = bigint 2020L
    let bigDeals = bigRepeastAndSimplify deckSize deals reps

    let [|Increment inc; Cut cut; |] = bigDeals
    // let cut = if cut < 0L then deckSize + cut else cut
    printfn "bigInc: %A, bigCut: %A" inc cut

    // // let preStackPos = (deckSize - 1L) - finalPos
    // // printfn "preStackPos: %i" preStackPos

    let preCutPos = reverseCut deckSize cut finalPos
    printfn "preCutPos: %A" preCutPos

    let checkFinal = applyCut deckSize cut preCutPos
    printfn "cut check, expect:%A, got:%A" finalPos checkFinal

    let increments = increments deckSize inc bigint.One |> List.ofSeq
    // printfn "Increments: %A" increments

    let decon = decon increments (preCutPos + bigint.One) |> List.ofSeq
    // printfn "Decon: %A" decon       ^^^ why add one? ^^^

    let estimate = (decon |> Seq.sumBy snd) % deckSize
    printfn "Estimate: %A" estimate

    applyIncs deckSize inc estimate
    |> applyCut deckSize cut
