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


testEntryNumberAt : Test
testEntryNumberAt =
    describe "Entry.entryNumberAt"
        [ test "non-start square" <|
            \_ ->
                let
                    entryListings =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                            |> Result.map Entry.allFromGrid

                    input =
                        Result.map
                            (\e -> Entry.entryNumberAt e ( 2, 3 ))
                            entryListings
                in
                    Expect.equal input (Ok Nothing)
        , test "across-only square" <|
            \_ ->
                let
                    entryListings =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                            |> Result.map Entry.allFromGrid

                    input =
                        Result.map
                            (\e -> Entry.entryNumberAt e ( 0, 1 ))
                            entryListings
                in
                    Expect.equal input
                        (Ok <| Just 5)
        , test "down-only square" <|
            \_ ->
                let
                    entryListings =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                            |> Result.map Entry.allFromGrid

                    input =
                        Result.map
                            (\e -> Entry.entryNumberAt e ( 1, 0 ))
                            entryListings
                in
                    Expect.equal input (Ok <| Just 2)
        , test "across and down square" <|
            \_ ->
                let
                    entryListings =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                            |> Result.map Entry.allFromGrid

                    input =
                        Result.map
                            (\e -> Entry.entryNumberAt e ( 0, 0 ))
                            entryListings
                in
                    Expect.equal input (Ok <| Just 1)
        ]


testAllFromGrid : Test
testAllFromGrid =
    describe "Entry.allFromGrid"
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


testEntryMembershipsFrom : Test
testEntryMembershipsFrom =
    describe "Entry.entryMembershipsFrom"
        [ test "four-by-four Grid" <|
            \_ ->
                let
                    expectedOutput =
                        Ok
                            [ ( ( 0, 0 ), [ 1, 1 ] )
                            , ( ( 0, 1 ), [ 5, 1 ] )
                            , ( ( 0, 2 ), [ 7, 1 ] )
                            , ( ( 0, 3 ), [] )
                            , ( ( 1, 0 ), [ 1, 2 ] )
                            , ( ( 1, 1 ), [ 5, 2 ] )
                            , ( ( 1, 2 ), [] )
                            , ( ( 1, 3 ), [ 9, 9 ] )
                            , ( ( 2, 0 ), [ 1, 3 ] )
                            , ( ( 2, 1 ), [] )
                            , ( ( 2, 2 ), [ 8, 8 ] )
                            , ( ( 2, 3 ), [ 9, 8 ] )
                            , ( ( 3, 0 ), [ 1, 4 ] )
                            , ( ( 3, 1 ), [ 6, 4 ] )
                            , ( ( 3, 2 ), [ 8, 4 ] )
                            , ( ( 3, 3 ), [ 9, 4 ] )
                            ]

                    grid =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"

                    entryListings =
                        Result.map Entry.allFromGrid grid

                    input =
                        Result.map2 Entry.entryMembershipsFromEntryListings grid entryListings
                            |> Result.map Entry.flattenEntryMemberships
                in
                    Expect.equal input expectedOutput
        ]
