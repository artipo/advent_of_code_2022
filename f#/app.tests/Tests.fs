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
        |> snd
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
        |> Seq.sumBy snd
        |> should equal 45000u