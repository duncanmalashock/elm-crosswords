module EntryTests exposing (..)

import Entry exposing (EntryStart(..))
import Grid
import Test exposing (Test, describe, test, skip)
import Expect


suite : Test
suite =
    describe "Entry"
        [ describe "allFromGrid"
            [ test "one-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 {} {}
                                ]

                        input =
                            Grid.fromString 1 1 "."
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 {} {}
                                , DownOnly 2 {}
                                ]

                        input =
                            Grid.fromString 2 1 ".."
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid with a block" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 {} {}
                                ]

                        input =
                            Grid.fromString 2 1 ".*"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 {} {}
                                , DownOnly 2 {}
                                , AcrossOnly 3 {}
                                ]

                        input =
                            Grid.fromString 2 2 "...."
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-by-four Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                ([ AcrossAndDown 1 {} {}
                                 , DownOnly 2 {}
                                 , DownOnly 3 {}
                                 , DownOnly 4 {}
                                 , AcrossOnly 5 {}
                                 , AcrossOnly 6 {}
                                 , AcrossOnly 7 {}
                                 , AcrossAndDown 8 {} {}
                                 , AcrossAndDown 9 {} {}
                                 ]
                                )

                        input =
                            Grid.fromString 4 4 "......*..*..*..."
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            ]
        ]
