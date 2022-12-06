module App.Solutions.Day05

open System.Collections.Generic
open App.Helpers
open FSharpPlus
open FsToolkit.ErrorHandling
open System.Text.RegularExpressions

// domain
type CrateName = char
type StackIndex = uint
type Stack = List<CrateName>

let cratesNameRegex = Regex(@"\[(?<name>\w{1})\]", RegexOptions.Multiline)
let stacksIndexRegex = Regex(@"(?<index>\d{1})", RegexOptions.Multiline)
let movementRegex = Regex(@"^move (?<count>\d+) from (?<from>\d{1}) to (?<to>\d{1})$", RegexOptions.Multiline)

let getValue (name : string) (m : Match) =
    m.Groups.[name].Value

let parseCrateNames (line : string) =
    cratesNameRegex.Matches(line)
    |> Seq.map (fun m -> m.Index / 4, m |> getValue "name" |> char) // return crate i and name

let parseStackIndexes (line : string) =
    stacksIndexRegex.Matches(line)
    |> Seq.map (fun m -> m.Index / 4, m |> getValue "index" |> uint) // return stack i and index

let parseMovement (line : string) =
    movementRegex.Match(line)
    |> (fun m ->
        // tuple (count * from * to)
        m |> getValue "count" |> int,
        m |> getValue "from" |> uint,
        m |> getValue "to" |> uint)

let biggestStack (stacks : List<_> seq) =
    stacks
    |> Seq.map (fun s -> s.Count)
    |> Seq.max

let skipParsedLines num (lines : string seq) =
    lines
    |> Seq.skip num

let parseStacks (lines : string seq) =
    
    // names
    let stacks = Dictionary<int, List<CrateName>>()
    
    let addCrate (index, crate) =
        if not(stacks.ContainsKey(index)) then
            stacks.Add(index, List<_>())
        stacks.[index].Insert(0, crate)
    
    lines
    |> Seq.map parseCrateNames
    |> Seq.iter (Seq.iter addCrate)
    
    let maxCrateNumber = biggestStack stacks.Values
    
    let remainingLines =
        lines
        |> skipParsedLines maxCrateNumber
    
    // indexes
    let stacksByIndex =
        remainingLines
        |> Seq.head
        |> parseStackIndexes
        |> Seq.map (fun (i, index) -> index, stacks.[i])
        |> Map.ofSeq
    
    let remainingLines =
        remainingLines
        |> skipParsedLines 2
        
    stacksByIndex, remainingLines

let moveCrateToStack (fromStack : Stack) (toStack : Stack) fromIndex =
    toStack.Add(fromStack.[fromIndex])
    fromStack.RemoveAt(fromIndex)
    ()

let crateIndex9000 (_i : int) (_count : int) (stackCount : int) =
    stackCount - 1

let crateIndex9001 i count stackCount =
    stackCount - count + i

let parseStacksAndApplyMovements indexFinder (lines : string seq)  =
    let stacks, remainingLines = parseStacks lines
    
    // movements
    remainingLines
    |> Seq.map parseMovement
    |> Seq.iter (fun (count, fromIndex, toIndex) ->
        for i in [ 0 .. (count - 1) ] do
            let fromStack = stacks.[fromIndex]
            let toStack = stacks.[toIndex]
            let fromCrateIndex = indexFinder i count fromStack.Count
            moveCrateToStack fromStack toStack fromCrateIndex)
    
    stacks

let parseStacksAndApplyMovements9000 : seq<string> -> Map<uint,List<CrateName>> = parseStacksAndApplyMovements crateIndex9000
let parseStacksAndApplyMovements9001 : seq<string> -> Map<uint,List<CrateName>> = parseStacksAndApplyMovements crateIndex9001

let getStacksTopCrateAfterMovements9000 (lines : string seq) =
    lines
    |> parseStacksAndApplyMovements9000
    |> Map.toSeq
    |> Seq.map (snd >> Seq.last >> string)

let getStacksTopCrateAfterMovements9001 (lines : string seq) =
    lines
    |> parseStacksAndApplyMovements9001
    |> Map.toSeq
    |> Seq.map (snd >> Seq.last >> string)
    
// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day06.txt"
    |> getStacksTopCrateAfterMovements9000
    |> String.concat ""
    |> printfn "Day05, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day05.txt"
    |> getStacksTopCrateAfterMovements9001
    |> String.concat ""
    |> printfn "Day05, puzzle 2 -> result = %A"