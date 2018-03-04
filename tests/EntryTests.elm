module EntryTests exposing (..)

import Entry exposing (EntryStart(..))
import Grid
import Test exposing (Test, describe, test, skip)
import Expect


flattenResult =
    Result.map
        (Entry.allFromGrid
            >> (\l -> (Entry.acrossList l) ++ (Entry.downList l))
        )


suite : Test
suite =
    describe "Entry"
        [ describe "allFromGrid"
            [ test "one-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "A" )
                                , ( 1, "A" )
                                ]

                        input =
                            Grid.fromString 1 1 "A"
                                |> flattenResult
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "AB" )
                                , ( 1, "A" )
                                , ( 2, "B" )
                                ]

                        input =
                            Grid.fromString 2 1 "AB"
                                |> flattenResult
                    in
                        Expect.equal input expectedOutput
            , test "two-squared Grid with a block" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "C" )
                                , ( 1, "C" )
                                ]

                        input =
                            Grid.fromString 2 1 "C*"
                                |> flattenResult
                    in
                        Expect.equal input expectedOutput
            , test "four-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "DO" )
                                , ( 3, "AH" )
                                , ( 1, "DA" )
                                , ( 2, "OH" )
                                ]

                        input =
                            Grid.fromString 2 2 "DOAH"
                                |> flattenResult
                    in
                        Expect.equal input expectedOutput
            , test "four-by-four Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "ABCD" )
                                , ( 5, "EF" )
                                , ( 6, "G" )
                                , ( 7, "H" )
                                , ( 8, "IJ" )
                                , ( 9, "KLM" )
                                , ( 1, "AEH" )
                                , ( 2, "BF" )
                                , ( 3, "C" )
                                , ( 4, "DGJM" )
                                , ( 8, "IL" )
                                , ( 9, "K" )
                                ]

                        input =
                            Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                                |> flattenResult
                    in
                        Expect.equal input expectedOutput
            ]
        ]
