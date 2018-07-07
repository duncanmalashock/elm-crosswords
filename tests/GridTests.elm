module GridTests exposing (..)

import Grid exposing (Square(..), StartsEntries(..))
import Direction exposing (Direction(..))
import Test exposing (Test, describe, test, skip)
import Expect


testBlank : Test
testBlank =
    describe "Grid.blank"
        [ test "is filled with blank letterSquares" <|
            \_ ->
                let
                    expectedOutput =
                        Ok "  \n  "

                    input =
                        Grid.blank 2 2
                            |> Result.map Grid.toString
                in
                    Expect.equal input expectedOutput
        ]


testToRows : Test
testToRows =
    describe "Grid.toRows"
        [ test "returns a nested list of rows with coordinates" <|
            \_ ->
                let
                    blankGrid =
                        Grid.blank 2 2

                    expectedOutput =
                        Ok
                            [ [ ( ( 0, 0 )
                                , LetterSquare ( 0, 0 )
                                    ' '
                                    { startsEntries = StartsAcrossAndDown
                                    , inAcrossEntry = 1
                                    , inDownEntry = 1
                                    }
                                )
                              , ( ( 1, 0 )
                                , LetterSquare ( 1, 0 )
                                    ' '
                                    { startsEntries = StartsDown
                                    , inAcrossEntry = 1
                                    , inDownEntry = 2
                                    }
                                )
                              ]
                            , [ ( ( 0, 1 )
                                , LetterSquare ( 0, 1 )
                                    ' '
                                    { startsEntries = StartsAcross
                                    , inAcrossEntry = 3
                                    , inDownEntry = 1
                                    }
                                )
                              , ( ( 1, 1 )
                                , LetterSquare ( 1, 1 )
                                    ' '
                                    { startsEntries = NoStart
                                    , inAcrossEntry = 3
                                    , inDownEntry = 2
                                    }
                                )
                              ]
                            ]
                in
                    Expect.equal (Result.map Grid.toRows blankGrid) expectedOutput
        ]



--
-- testIsAcrossEntry : Test
-- testIsAcrossEntry =
--     describe "Grid.isAcrossEntryStart"
--         [ test "returns true if a coordinate begins an across entry" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok True
--
--                     input =
--                         Grid.fromString 2 2 ".**."
--                             |> Result.map
--                                 (\g -> Grid.isAcrossEntryStart g ( 0, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns false if a coordinate does not begin an across entry" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok False
--
--                     input =
--                         Grid.fromString 2 2 "...."
--                             |> Result.map
--                                 (\g -> Grid.isAcrossEntryStart g ( 1, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns false if a coordinate is not a letter square" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok True
--
--                     input =
--                         Grid.fromString 2 2 "****"
--                             |> Result.map
--                                 (\g -> Grid.isAcrossEntryStart g ( 0, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         ]
--
--
-- testIsDownEntry : Test
-- testIsDownEntry =
--     describe "Grid.isDownEntryStart"
--         [ test "returns true if a coordinate begins a down entry" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok True
--
--                     input =
--                         Grid.fromString 2 2 ".**."
--                             |> Result.map
--                                 (\g -> Grid.isDownEntryStart g ( 0, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns false if a coordinate does not begin a down entry" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok False
--
--                     input =
--                         Grid.fromString 2 2 "...."
--                             |> Result.map
--                                 (\g -> Grid.isDownEntryStart g ( 0, 1 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns false if a coordinate is not a letter square" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok True
--
--                     input =
--                         Grid.fromString 2 2 "****"
--                             |> Result.map
--                                 (\g -> Grid.isDownEntryStart g ( 0, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         ]
--
--
--
-- testSquareAtCoordinate : Test
-- testSquareAtCoordinate =
--     describe "Grid.squareAtCoordinate"
--         [ test "returns Nothing if out of bounds" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok Nothing
--
--                     input =
--                         Grid.fromString 2 2 "ABCD"
--                             |> Result.map
--                                 (\g -> Grid.squareAtCoordinate g ( 3, 3 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns a letter square at matching coordinate" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok
--                             (Just <|
--                                 LetterSquare 'B'
--                                     { startsEntries = StartsAcrossAndDown
--                                     , inAcrossEntry = 1
--                                     , inDownEntry = 2
--                                     }
--                             )
--
--                     input =
--                         Grid.fromString 2 2 "ABCD"
--                             |> Result.map
--                                 (\g -> Grid.squareAtCoordinate g ( 1, 0 ))
--                 in
--                     Expect.equal input expectedOutput
--         , test "returns a block square at matching coordinate" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok (Just <| BlockSquare)
--
--                     input =
--                         Grid.fromString 2 2 "ABC*"
--                             |> Result.map
--                                 (\g -> Grid.squareAtCoordinate g ( 1, 1 ))
--                 in
--                     Expect.equal input expectedOutput
--         ]
--
--
-- testWidth : Test
-- testWidth =
--     describe "Grid.width"
--         [ test "returns the number of columns for a valid grid" <|
--             \_ ->
--                 let
--                     grid =
--                         Grid.fromString 2 2 "...."
--                 in
--                     Expect.equal (Result.map Grid.width grid) <| Ok 2
--         ]
--
--
-- testHeight : Test
-- testHeight =
--     describe "Grid.height"
--         [ test "returns the number of rows for a valid grid" <|
--             \_ ->
--                 let
--                     grid =
--                         Grid.fromString 2 2 "...."
--                 in
--                     Expect.equal (Result.map Grid.height grid) <| Ok 2
--         ]
--
--
-- testFromString : Test
-- testFromString =
--     describe "Grid.fromString"
--         [ test "one-squared Grid" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok
--                             [ LetterSquare ' '
--                                 { startsEntries = StartsAcrossAndDown
--                                 , inAcrossEntry = 1
--                                 , inDownEntry = 1
--                                 }
--                             ]
--
--                     input =
--                         "."
--                 in
--                     Expect.equal
--                         (Grid.fromString 1 1 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         , test "two-squared Grid" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok
--                             [ LetterSquare ' '
--                                 { startsEntries = StartsAcrossAndDown
--                                 , inAcrossEntry = 1
--                                 , inDownEntry = 1
--                                 }
--                             , LetterSquare ' '
--                                 { startsEntries = StartsDown
--                                 , inAcrossEntry = 1
--                                 , inDownEntry = 2
--                                 }
--                             ]
--
--                     input =
--                         ".."
--                 in
--                     Expect.equal
--                         (Grid.fromString 2 1 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         , test "four-squared Grid" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Ok
--                             [ LetterSquare ' '
--                                 { startsEntries = StartsAcrossAndDown
--                                 , inAcrossEntry = 1
--                                 , inDownEntry = 1
--                                 }
--                             , BlockSquare
--                             , BlockSquare
--                             , LetterSquare ' '
--                                 { startsEntries = StartsAcrossAndDown
--                                 , inAcrossEntry = 1
--                                 , inDownEntry = 1
--                                 }
--                             ]
--
--                     input =
--                         ".**."
--                 in
--                     Expect.equal
--                         (Grid.fromString 2 2 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         , test "invalid characters" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Err "Invalid character"
--
--                     input =
--                         "&$()"
--                 in
--                     Expect.equal
--                         (Grid.fromString 2 2 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         , test "too few characters" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Err "2 is too few characters for a 2x2 Grid"
--
--                     input =
--                         ".."
--                 in
--                     Expect.equal
--                         (Grid.fromString 2 2 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         , test "too many characters" <|
--             \_ ->
--                 let
--                     expectedOutput =
--                         Err "10 is too many characters for a 3x3 Grid"
--
--                     input =
--                         ".........."
--                 in
--                     Expect.equal
--                         (Grid.fromString 3 3 input
--                             |> flattenResult
--                         )
--                         expectedOutput
--         ]
