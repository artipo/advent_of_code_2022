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