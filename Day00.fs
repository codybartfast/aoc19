module Day00
let day = "00"

//#nowarn "0025"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let display obj = (printfn "%O" obj); obj
let len (seq : seq<'a>) = Seq.length seq
let toChars (str : string) = str.ToCharArray()
let fromChars (chrs : char[]) = String(chrs)
let encode (str : string) = System.Text.Encoding.ASCII.GetBytes(str);
let toHex = (BitConverter.ToString
            >> (fun str ->str.Replace("-", String.Empty)))
let groupValue (m:Match) (idx:int) = m.Groups.[idx].Value
let rxMatch pattern str = Regex.Match(str, pattern)
let rxMatches pattern str = Regex.Matches(str, pattern)
let rxSplit pattern str = Regex.Split(str, pattern)
let lnSplit str = rxSplit @"\r?\n" str
let (||~) pred1 pred2 = (fun a -> (pred1 a) || (pred2 a))
let (&&~) pred1 pred2 = (fun a -> (pred1 a) && (pred2 a))
let filterCount predicate = Seq.filter predicate >> Seq.length


let inputFile = Path.Combine("inputs", "input" + day + ".txt")
let lines = File.ReadAllLines(inputFile)
let input =
    let parseLine =  rxMatch "(.*)" >> fun mtch ->
        let grp idx = groupValue mtch idx
        let grpi = grp >> int
        grp 1
    //[| parseLine lines.[0] |]
    lines |> Array.map parseLine


(* ================ Part 1 ================ *)


let Part1 () =
    input


(* ================ Part 2 ================ *)


let Part2 () =
    ()
