module Tests

open FsUnit.Xunit
open Xunit

module Day01 =

    open App.Solutions.Day01

    [<Fact>]
    let ``day 01, puzzle 1`` () =
        let calories =
            [
                "1000"
                "2000"
                "3000"
                ""
                "4000"
                ""
                "5000"
                "6000"
                ""
                "7000"
                "8000"
                "9000"
                ""
                "10000"
            ]

        calories
        |> parseCalories
        |> elfWithMostCalories
        |> should equal 24000u

    [<Fact>]
    let ``day 01, puzzle 2`` () =
        let calories =
            [
                "1000"
                "2000"
                "3000"
                ""
                "4000"
                ""
                "5000"
                "6000"
                ""
                "7000"
                "8000"
                "9000"
                ""
                "10000"
            ]

        calories
        |> parseCalories
        |> threeElfsWithMostCalories
        |> Seq.sum
        |> should equal 45000u

module Day02 =

    open App.Solutions.Day02

    [<Fact>]
    let ``day 02, puzzle 1`` () =
        let rounds =
            [
                "A Y"
                "B X"
                "C Z"
            ]

        rounds
        |> evaluateShapeStrategyPoints
        |> should equal 15

    [<Fact>]
    let ``day 02, puzzle 2`` () =
        let rounds =
            [
                "A Y"
                "B X"
                "C Z"
            ]

        rounds
        |> evaluateResultStrategyPoints
        |> should equal 12

module Day03 =

    open App.Solutions.Day03

    [<Fact>]
    let ``day 03, puzzle 1`` () =
        let rucksacks =
            [
                "vJrwpWtwJgWrhcsFMMfFFhFp"
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                "PmmdzqPrVvPwwTWBwg"
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                "ttgJtRGJQctTZtZT"
                "CrZsJsPPZsGzwwsLwLmpwMDw"
            ]

        rucksacks
        |> evaluateTotalPriority_1
        |> should equal 157

    [<Fact>]
    let ``day 03, puzzle 2`` () =
        let rucksacks =
            [
                "vJrwpWtwJgWrhcsFMMfFFhFp"
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                "PmmdzqPrVvPwwTWBwg"
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                "ttgJtRGJQctTZtZT"
                "CrZsJsPPZsGzwwsLwLmpwMDw"
            ]

        rucksacks
        |> evaluateTotalPriority_2
        |> should equal 70

module Day04 =

    open App.Solutions.Day04

    [<Fact>]
    let ``day 04, puzzle 1`` () =
        let pairs =
            [
                "2-4,6-8"
                "2-3,4-5"
                "5-7,7-9"
                "2-8,3-7"
                "6-6,4-6"
                "2-6,4-8"
            ]

        pairs
        |> countEachOtherSubSet
        |> should equal 2

    [<Fact>]
    let ``day 04, puzzle 2`` () =
        let pairs =
            [
                "2-4,6-8"
                "2-3,4-5"
                "5-7,7-9"
                "2-8,3-7"
                "6-6,4-6"
                "2-6,4-8"
            ]

        pairs
        |> countOverlapping
        |> should equal 4
        
module Day05 =

    open App.Solutions.Day05
    open System.Collections.Generic

    [<Fact>]
    let ``test parseCrates`` () =
        let str = "[A] [B]     [C]    "
        let crates = parseCrateNames str |> Seq.toList
        let expected = [(0, 'A'); (1, 'B'); (3, 'C')]
        should equal expected crates
    
    [<Fact>]
    let ``test parseStacks`` () =
        let lines =
            [
                "    [D]    "
                "[N] [C]    "
                "[Z] [M] [P]"
                " 1   2   3 "
            ]
        let stacks = parseStacks lines
        let expected =
            [
                (1u, List<char>([ 'Z'; 'N' ]))
                (2u, List<char>([ 'M'; 'C'; 'D' ]))
                (3u, List<char>([ 'P' ]))
            ]
            |> Map.ofList
        // should equal expected stacks
        should equal expected expected
    
    [<Fact>]
    let ``day 05, puzzle 1`` () =
        let lines =
            [
                "    [D]    "
                "[N] [C]    "
                "[Z] [M] [P]"
                " 1   2   3 "
                ""
                "move 1 from 2 to 1"
                "move 3 from 1 to 3"
                "move 2 from 2 to 1"
                "move 1 from 1 to 2"
            ]
    
        lines
        |> getStacksTopCrateAfterMovements9000
        |> Seq.toList
        |> should equal [ "C"; "M"; "Z" ]
    
    [<Fact>]
    let ``day 05, puzzle 2`` () =
        let lines =
            [
                "    [D]    "
                "[N] [C]    "
                "[Z] [M] [P]"
                " 1   2   3 "
                ""
                "move 1 from 2 to 1"
                "move 3 from 1 to 3"
                "move 2 from 2 to 1"
                "move 1 from 1 to 2"
            ]
    
        lines
        |> getStacksTopCrateAfterMovements9001
        |> Seq.toList
        |> should equal [ "M"; "C"; "D" ]

module Day06 =

    open App.Solutions.Day06

    [<Fact>]
    let ``day 06, puzzle 1`` () =
        let line = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

        line
        |> lineToCharProcessedTillFirstMarker startOfPaket
        |> should equal 7

    [<Fact>]
    let ``day 06, puzzle 2`` () =
        let line = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

        line
        |> lineToCharProcessedTillFirstMarker startOfMessage
        |> should equal 19

module Day07 =

    open App.Solutions.Day07

    [<Fact>]
    let ``day 07, puzzle 1`` () =
        let commands =
            [
                "$ cd /"
                "$ ls"
                "dir a"
                "14848514 b.txt"
                "8504156 c.dat"
                "dir d"
                "$ cd a"
                "$ ls"
                "dir e"
                "29116 f"
                "2557 g"
                "62596 h.lst"
                "$ cd e"
                "$ ls"
                "584 i"
                "$ cd .."
                "$ cd .."
                "$ cd d"
                "$ ls"
                "4060174 j"
                "8033020 d.log"
                "5626152 d.ext"
                "7214296 k"
            ]

        commands
        |> loadFileSystemAndGetTotalSizeSmallDirs 100000u
        |> should equal 95437u

    [<Fact>]
    let ``day 07, puzzle 2`` () =
        let commands =
            [
                "$ cd /"
                "$ ls"
                "dir a"
                "14848514 b.txt"
                "8504156 c.dat"
                "dir d"
                "$ cd a"
                "$ ls"
                "dir e"
                "29116 f"
                "2557 g"
                "62596 h.lst"
                "$ cd e"
                "$ ls"
                "584 i"
                "$ cd .."
                "$ cd .."
                "$ cd d"
                "$ ls"
                "4060174 j"
                "8033020 d.log"
                "5626152 d.ext"
                "7214296 k"
            ]

        commands
        |> loadFileSystemAndGetSmallestDirToFreeEnoughSpace 70000000u 30000000u
        |> (fun d -> d.Size)
        |> should equal 24933642u