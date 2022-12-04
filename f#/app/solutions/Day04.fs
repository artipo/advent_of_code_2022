module App.Solutions.Day04

open App.Helpers
open FSharpPlus
open FsToolkit.ErrorHandling

// domain
type ID = uint
type IDs = Set<ID>

let explodeIDs IDs =
    let s, e = IDs
    seq { s .. e }
    |> Set.ofSeq

let parseIDs text =
    text
    |> (String.split [ "-" ] >> Seq.toList)
    |> (function
        | [s; e] -> (uint s, uint e) |> explodeIDs |> Some
        | _ -> None)

let parseIDsPair (lines : string seq) =
    lines
    |> Seq.map (String.split [ "," ] >> Seq.toList)
    |> Seq.map (function
        | [left; right] ->
            option {
                let! leftIDs = parseIDs left
                let! rightIDs = parseIDs right
                return leftIDs, rightIDs
            }
        | _ -> None)
    |> Seq.choose id
    
let areEachOtherSubSet (left, right) =
    let [ min; max ] = [ left; right ] |> List.sortBy Set.count
    Set.isSubset min max

let doOverlap (left, right) =
    Set.intersect left right
    |> (<>) Set.empty

let countEachOtherSubSet lines =
    lines
    |> parseIDsPair
    |> Seq.filter areEachOtherSubSet
    |> Seq.length

let countOverlapping lines =
    lines
    |> parseIDsPair
    |> Seq.filter doOverlap
    |> Seq.length

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day04.txt"
    |> countEachOtherSubSet
    |> printfn "Day04, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day04.txt"
    |> countOverlapping
    |> printfn "Day04, puzzle 2 -> result = %A"