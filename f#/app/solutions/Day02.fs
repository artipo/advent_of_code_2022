module App.Solutions.Day02

open App.Helpers
open FSharpPlus

// domain
type Shape =
    | Rock
    | Paper
    | Scissor

type RoundResult =
    | Lost
    | Draw
    | Won

let parseEnemyShape =
    function
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissor

let parseShapeStrategy =
    function
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissor

let parseResultStrategy =
    function
        | "X" -> Lost
        | "Y" -> Draw
        | "Z" -> Won

let shapeToPoints =
    function
        | Rock -> 1
        | Paper -> 2
        | Scissor -> 3

let resultToPoints =
    function
        | Lost -> 0
        | Draw -> 3
        | Won -> 6

let evaluateResult (enemy, shape) =
    match enemy, shape with
    
    | Rock, Scissor -> Lost
    | Paper, Rock -> Lost
    | Scissor, Paper -> Lost
    
    | Rock, Rock -> Draw
    | Paper,Paper -> Draw
    | Scissor, Scissor -> Draw
    
    | Rock, Paper -> Won
    | Paper, Scissor -> Won
    | Scissor, Rock -> Won

let evaluateShape (enemy, result) =
    match enemy, result with
    
    | Rock, Lost -> Scissor
    | Rock, Draw -> Rock
    | Rock, Won -> Paper
    
    | Paper, Lost -> Rock
    | Paper, Draw -> Paper
    | Paper, Won -> Scissor
    
    | Scissor, Lost -> Paper
    | Scissor, Draw -> Scissor
    | Scissor, Won -> Rock
    
let parseRounds mapStrategy (lines : string seq) =
    lines
    |> Seq.map (String.split [ " " ] >> Seq.toList)
    |> Seq.map (function
        | [enemy; strategy] -> Some (parseEnemyShape enemy, mapStrategy strategy)
        | _ -> None)
    |> Seq.choose id

let evaluateRoundPoints resultEvaluator shapeEvaluator round =
    let resultPoints =
        round
        |> resultEvaluator
        |> resultToPoints
    let handSignPoints =
        round
        |> shapeEvaluator
        |> shapeToPoints
    resultPoints + handSignPoints

let evaluateShapeStrategyPoints lines =
    lines
    |> parseRounds parseShapeStrategy
    |> Seq.map (evaluateRoundPoints evaluateResult snd)
    |> Seq.sum

let evaluateResultStrategyPoints lines =
    lines
    |> parseRounds parseResultStrategy
    |> Seq.map (evaluateRoundPoints snd evaluateShape)
    |> Seq.sum

// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day02.txt"
    |> evaluateShapeStrategyPoints
    |> printfn "Day02, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day02.txt"
    |> evaluateResultStrategyPoints
    |> printfn "Day02, puzzle 2 -> result = %A"