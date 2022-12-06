module App.Solutions.Day06

open App.Helpers
open FSharpPlus
open FsToolkit.ErrorHandling

// domain
let startOfPaket = 4
let startOfMessage = 14

let signalToPossibleMarkers size line =
    line
    |> Seq.windowed size
    |> Seq.mapi (fun i w -> i, w)

let searchMarkers size ws =
    ws
    |> Seq.where (fun (i, w) -> (w |> Set.ofSeq |> Seq.length) = size)

let searchFirstMarker size ws =
    ws
    |> searchMarkers size
    |> Seq.minBy fst

let charProcessedToFirstMarker size marker =
    marker
    |> fst
    |> (+) size

let lineToCharProcessedTillFirstMarker size line =
    line
    |> signalToPossibleMarkers size
    |> searchFirstMarker size
    |> charProcessedToFirstMarker size
    
// wrappers
let solve_puzzle_1 () =
    File.readAllText @"./inputs/day06.txt"
    |> lineToCharProcessedTillFirstMarker startOfPaket
    |> printfn "Day06, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readAllText @"./inputs/day06.txt"
    |> lineToCharProcessedTillFirstMarker startOfMessage
    |> printfn "Day06, puzzle 2 -> result = %A"