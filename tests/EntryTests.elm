module EntryTests exposing (..)

import Entry exposing (Entry(..), Direction(..))
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
                            [ Entry 1 Across
                            , Entry 1 Down
                            ]

                        input =
                            Grid.fromString 1 1 "."
                                |> Result.withDefault []
                                |> Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            [ Entry 1 Across
                            , Entry 1 Down
                            , Entry 2 Down
                            ]

                        input =
                            Grid.fromString 2 1 ".."
                                |> Result.withDefault []
                                |> Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid with a block" <|
                \_ ->
                    let
                        expectedOutput =
                            [ Entry 1 Across
                            , Entry 1 Down
                            ]

                        input =
                            Grid.fromString 2 1 ".*"
                                |> Result.withDefault []
                                |> Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            [ Entry 1 Across
                            , Entry 1 Down
                            , Entry 2 Down
                            , Entry 3 Across
                            ]

                        input =
                            Grid.fromString 2 2 "...."
                                |> Result.withDefault []
                                |> Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-by-four Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            [ Entry 1 Across
                            , Entry 1 Down
                            , Entry 2 Down
                            , Entry 3 Across
                            ]

                        input =
                            Grid.fromString 4 4 "......*..*..*..."
                                |> Result.withDefault []
                                |> Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            ]
        ]
