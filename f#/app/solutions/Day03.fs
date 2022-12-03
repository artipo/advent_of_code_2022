module App.Solutions.Day03

open App

// domain
type Rucksack = string

let splitRucksack (line : Rucksack) =
    let compartmentSize = line.Length / 2
    let firstCompartment = line.Substring(0, compartmentSize)
    let lastCompartment = line.Substring(compartmentSize)
    firstCompartment, lastCompartment

let searchItemTypeError rucksack =
    let first, last = splitRucksack rucksack
    let firstSet = first |> Set.ofSeq
    let lastSet = last |> Set.ofSeq
    
    firstSet
    |> Set.intersect lastSet
    |> Set.exactlyOne

let searchBadgeItemType rucksacks =
    rucksacks
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.exactlyOne

let getItemTypePriority (itemType : char) =
    let isUpper = (int itemType) < (int 'a')
    let charBase, intValueBase = if isUpper then 'A', 27 else 'a', 1
    
    (int itemType) - (int charBase) + intValueBase

let evaluateTotalPriority_1 (lines : string seq) =
    lines
    |> Seq.map searchItemTypeError
    |> Seq.map getItemTypePriority
    |> Seq.sum

let evaluateTotalPriority_2 (lines : string seq) =
    lines
    |> Seq.chunkBySize 3
    |> Seq.map searchBadgeItemType
    |> Seq.map getItemTypePriority
    |> Seq.sum

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day03.txt"
    |> evaluateTotalPriority_1
    |> printfn "Day03, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day03.txt"
    |> evaluateTotalPriority_2
    |> printfn "Day03, puzzle 2 -> result = %A"