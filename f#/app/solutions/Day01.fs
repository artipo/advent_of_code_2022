module App.Solutions.Day01

open System
open App.Helpers
open FSharpPlus

// domain
type Calories = uint

let parseCalories (lines : string seq) =
    lines
    |> Seq.map (Option.ofTryParse UInt32.TryParse)
    |> Seq.split [ [ None ] ]
    |> Seq.map ((Seq.choose id) >> Seq.sum)

let orderByCalories (calories : Calories seq) =
    calories
    |> Seq.sortDescending

let elfWithMostCalories (calories : Calories seq) =
    calories
    |> orderByCalories
    |> Seq.head

let threeElfsWithMostCalories (calories : Calories seq) =
    calories
    |> orderByCalories
    |> Seq.take 3

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day01.txt"
    |> parseCalories
    |> elfWithMostCalories
    |> printfn "Day01, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day01.txt"
    |> parseCalories
    |> threeElfsWithMostCalories
    |> Seq.sum
    |> printfn "Day01, puzzle 2 -> result = %A"