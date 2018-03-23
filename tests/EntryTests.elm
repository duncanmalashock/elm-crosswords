module EntryTests exposing (..)

import Entry exposing (EntryStart(..))
import Direction exposing (Direction(..))
import Grid
import Test exposing (Test, describe, test, skip)
import Expect


flattenResult =
    Result.map
        (Entry.entryStartDictFromGrid
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
                            |> Result.map Entry.entryStartDictFromGrid

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
                            |> Result.map Entry.entryStartDictFromGrid

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
                            |> Result.map Entry.entryStartDictFromGrid

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
                            |> Result.map Entry.entryStartDictFromGrid

                    input =
                        Result.map
                            (\e -> Entry.entryNumberAt e ( 0, 0 ))
                            entryListings
                in
                    Expect.equal input (Ok <| Just 1)
        ]


testentryStartDictFromGrid : Test
testentryStartDictFromGrid =
    describe "Entry.entryStartDictFromGrid"
        [ test "one-squared Grid" <|
            \_ ->
                let
                    expectedOutput =
                        Ok
                            [ ( ( 0, 0 ), Entry.entry 1 Across "A" "" )
                            , ( ( 0, 0 ), Entry.entry 1 Down "A" "" )
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
                            [ ( ( 0, 0 ), Entry.entry 1 Across "AB" "" )
                            , ( ( 0, 0 ), Entry.entry 1 Down "A" "" )
                            , ( ( 1, 0 ), Entry.entry 2 Down "B" "" )
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
                            [ ( ( 0, 0 ), Entry.entry 1 Across "C" "" )
                            , ( ( 0, 0 ), Entry.entry 1 Down "C" "" )
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
                            [ ( ( 0, 0 ), Entry.entry 1 Across "DO" "" )
                            , ( ( 0, 1 ), Entry.entry 3 Across "AH" "" )
                            , ( ( 0, 0 ), Entry.entry 1 Down "DA" "" )
                            , ( ( 1, 0 ), Entry.entry 2 Down "OH" "" )
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
                            ([ ( ( 0, 0 ), Entry.entry 1 Across "ABCD" "" )
                             , ( ( 0, 1 ), Entry.entry 5 Across "EF" "" )
                             , ( ( 3, 1 ), Entry.entry 6 Across "G" "" )
                             , ( ( 0, 2 ), Entry.entry 7 Across "H" "" )
                             , ( ( 2, 2 ), Entry.entry 8 Across "IJ" "" )
                             , ( ( 1, 3 ), Entry.entry 9 Across "KLM" "" )
                             , ( ( 0, 0 ), Entry.entry 1 Down "AEH" "" )
                             , ( ( 1, 0 ), Entry.entry 2 Down "BF" "" )
                             , ( ( 2, 0 ), Entry.entry 3 Down "C" "" )
                             , ( ( 3, 0 ), Entry.entry 4 Down "DGJM" "" )
                             , ( ( 2, 2 ), Entry.entry 8 Down "IL" "" )
                             , ( ( 1, 3 ), Entry.entry 9 Down "K" "" )
                             ]
                            )

                    input =
                        Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                            |> flattenResult
                in
                    Expect.equal input expectedOutput
        ]


testEntryMembershipDictFrom : Test
testEntryMembershipDictFrom =
    describe "Entry.EntryMembershipDictFrom"
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
                        Result.map Entry.entryStartDictFromGrid grid

                    input =
                        Result.map2 Entry.entryMembershipDictFromEntryStartDict grid entryListings
                            |> Result.map Entry.flattenEntryMembershipDict
                in
                    Expect.equal input expectedOutput
        ]
