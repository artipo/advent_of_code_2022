module App.Solutions.Day01

open System
open App.Helpers
open FSharpPlus

// domain
type SnackCalories = uint
type Elf = uint
type CaloriesCarriedByElf = Elf * SnackCalories

let mapToCalories i (calories : SnackCalories option seq) : CaloriesCarriedByElf =
    let elf =
        i + 1
        |> uint
    let totalCalories =
        calories
        |> Seq.choose id
        |> Seq.sum
    elf, totalCalories

let parseCalories (lines : string seq) =
    lines
    |> Seq.map (Option.ofTryParse UInt32.TryParse)
    |> Seq.split [ [ None ] ]
    |> Seq.mapi mapToCalories

let orderByCalories (elfs : CaloriesCarriedByElf seq) =
    elfs
    |> Seq.sortByDescending snd

let elfWithMostCalories (elfs : CaloriesCarriedByElf seq) =
    elfs
    |> orderByCalories
    |> Seq.head

let threeElfsWithMostCalories (elfs : CaloriesCarriedByElf seq) =
    elfs
    |> orderByCalories
    |> Seq.take 3

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day01.txt"
    |> parseCalories
    |> elfWithMostCalories
    |> snd
    |> printfn "Day01, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day01.txt"
    |> parseCalories
    |> threeElfsWithMostCalories
    |> Seq.sumBy snd
    |> printfn "Day01, puzzle 2 -> result = %A"