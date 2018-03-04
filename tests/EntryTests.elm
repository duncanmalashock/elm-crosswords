module EntryTests exposing (..)

import Entry exposing (EntryStart(..))
import Grid
import Test exposing (Test, describe, test, skip)
import Expect


getEntries =
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
                                |> getEntries
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
                                |> getEntries
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
                                |> getEntries
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
                                |> getEntries
                    in
                        Expect.equal input expectedOutput
            , test "four-by-four Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ ( 1, "ABCD" )
                                , ( 5, "EF" )
                                , ( 7, "H" )
                                , ( 9, "KLM" )
                                , ( 8, "IJ" )
                                , ( 6, "G" )
                                , ( 1, "AEH" )
                                , ( 2, "BF" )
                                , ( 9, "K" )
                                , ( 3, "C" )
                                , ( 8, "IL" )
                                , ( 4, "DGJM" )
                                ]

                        input =
                            Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                                |> getEntries
                    in
                        Expect.equal input expectedOutput
            ]
        ]
