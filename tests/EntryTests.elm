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
                                ([ AcrossAndDown 1 "BING" "BYE"
                                 , DownOnly 2 "IN"
                                 , DownOnly 3 "N"
                                 , DownOnly 4 "GOLF"
                                 , AcrossOnly 5 "YN"
                                 , AcrossOnly 6 "O"
                                 , AcrossOnly 7 "E"
                                 , AcrossAndDown 8 "AL" "AI"
                                 , AcrossAndDown 9 "KIF" "K"
                                 ]
                                )

                        input =
                            Grid.fromString 4 4 "BINGYN*OE*AL*KIF"
                                |> Result.map Entry.allFromGrid
                    in
                        Expect.equal input expectedOutput
            ]
        ]
