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
                                [ AcrossAndDown 1 "A" "A"
                                ]

                        input =
                            Grid.fromString 1 1 "A"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 "AB" "A"
                                , DownOnly 2 "B"
                                ]

                        input =
                            Grid.fromString 2 1 "AB"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid with a block" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 "C" "C"
                                ]

                        input =
                            Grid.fromString 2 1 "C*"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ AcrossAndDown 1 "DO" "DA"
                                , DownOnly 2 "OH"
                                , AcrossOnly 3 "AH"
                                ]

                        input =
                            Grid.fromString 2 2 "DOAH"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            , test "four-by-four Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                ([ AcrossAndDown 1 "ABCD" "AEH"
                                 , DownOnly 2 "BF"
                                 , DownOnly 3 "C"
                                 , DownOnly 4 "DGJM"
                                 , AcrossOnly 5 "EF"
                                 , AcrossOnly 6 "G"
                                 , AcrossOnly 7 "H"
                                 , AcrossAndDown 8 "IJ" "IL"
                                 , AcrossAndDown 9 "KLM" "K"
                                 ]
                                )

                        input =
                            Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            ]
        ]
