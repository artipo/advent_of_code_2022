module App.Solutions.Day07

open App.Helpers
open FSharpPlus
open FsToolkit.ErrorHandling

// domain
type File =
    {
        Name : string
        Size : uint
    }

type Node =
    | File of File
    | Directory of Directory

and Directory =
    {
        Name : string
        Content : Node list
        Size : uint
    }

type Command =
    | Cd of string
    | Ls of string list

let parseCommand (lines : string list) =
    let cmd :: xs = lines
    match cmd.Substring(2, 2) with
    | "cd" -> Cd (cmd.Substring(5))
    | "ls" -> Ls xs

let parseCommands lines =
    let rec loop (lines : string list) cmdLines acc =
        match lines with
        | [] -> ((cmdLines |> List.rev |> parseCommand) :: acc) |> List.rev
        | x :: xs ->
            if x.StartsWith("$") then
                match cmdLines with
                | [] -> loop xs (x :: cmdLines) acc
                | _ -> loop lines [] ((cmdLines |> List.rev |> parseCommand) :: acc)
            else
                loop xs (x :: cmdLines) acc
    
    loop lines [] []

let parseFile (line : string) =
    let [| size; name |] = line.Split(" ")
    {
        Size = size |> uint
        Name = name
    }

let rec evaluateSize (node : Node) =
    match node with
    | File f -> f.Size
    | Directory d ->
        d.Content
        |> List.map evaluateSize
        |> List.sum

let commandsToRoot (cmds : Command list) =
    let rec loop cmds acc =
        match cmds with
        | [] -> acc |> List.rev, []
        | x :: xs ->
            match x with
            | Cd dirName when dirName = ".." -> acc |> List.rev, xs
            | Cd dirName ->
                let content, xs = loop xs []
                let dir =
                    { Name = dirName
                      Content = content
                      Size = content |> List.map evaluateSize |> List.sum }
                    |> Node.Directory
                loop xs (dir :: acc)
            | Ls rows ->
                let content =
                    rows
                    |> List.where (fun s -> s.StartsWith("dir") |> not)
                    |> List.map (parseFile >> Node.File)
                loop xs ((content |> List.rev) @ acc)
    
    loop cmds []
    |> fst
    |> List.head
    |> (function | Directory d -> d)

let listAllDirs (root : Directory) =
    let rec loop node =
        match node with
        | File _ -> []
        | Directory dir ->
            let subDirs =
                dir.Content
                |> List.map loop
                |> List.collect id
            dir :: subDirs
    
    loop (root |> Node.Directory)

let listSmallDirs size root =
    listAllDirs root
    |> List.where (fun d -> d.Size <= size)

let loadFileSystemAndGetTotalSizeSmallDirs size (lines : string seq) =
    lines
    |> List.ofSeq
    |> parseCommands
    |> commandsToRoot
    |> listSmallDirs size
    |> List.map (fun d -> d.Size)
    |> List.sum

let loadFileSystemAndGetSmallestDirToFreeEnoughSpace fileSystemDiskSpace updateDiskSpaceRequired (lines : string seq) =
    
    let root = 
        lines
        |> List.ofSeq
        |> parseCommands
        |> commandsToRoot
    
    // required - (total - root)
    let minimumDiskSpaceToFree = updateDiskSpaceRequired - (fileSystemDiskSpace - root.Size)
    
    root
    |> listAllDirs
    |> List.where (fun d -> d.Size > minimumDiskSpaceToFree)
    |> List.minBy (fun d -> d.Size)
    
// wrappers
let solve_puzzle_1 () =
    File.readLines @"./inputs/day07.txt"
    |> loadFileSystemAndGetTotalSizeSmallDirs 100000u
    |> printfn "Day07, puzzle 1 -> result = %A"

let solve_puzzle_2 () =
    File.readLines @"./inputs/day07.txt"
    |> loadFileSystemAndGetSmallestDirToFreeEnoughSpace 70000000u 30000000u
    |> (fun d -> d.Size)
    |> printfn "Day07, puzzle 2 -> result = %A"