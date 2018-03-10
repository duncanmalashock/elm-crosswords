module GridTests exposing (..)

import Grid exposing (Square(..))
import Test exposing (Test, describe, test, skip)
import Expect


flattenResult =
    Result.map (Grid.flatten)



-- TODO: tests for:
-- Grid.flatten
-- Grid.empty
-- Grid.blank
-- Grid.toRows
-- Grid.isAcrossEntryStart
-- Grid.isDownEntryStart
-- Grid.squareAtCoordinate
-- Grid.setAtCoordinate
-- Grid.squareAtRight
-- Grid.squareBelow


testWidth : Test
testWidth =
    describe "Grid.width"
        [ test "returns the number of columns for a valid grid" <|
            \_ ->
                let
                    grid =
                        Grid.fromString 2 2 "...."
                in
                    Expect.equal (Result.map Grid.width grid) <| Ok 2
        ]


testHeight : Test
testHeight =
    describe "Grid.height"
        [ test "returns the number of columns for a valid grid" <|
            \_ ->
                let
                    grid =
                        Grid.fromString 2 2 "...."
                in
                    Expect.equal (Result.map Grid.height grid) <| Ok 2
        ]


testFromString : Test
testFromString =
    describe "Grid.fromString"
        [ test "one-squared Grid" <|
            \_ ->
                let
                    expectedOutput =
                        Ok
                            [ LetterSquare ' '
                            ]

                    input =
                        "."
                in
                    Expect.equal
                        (Grid.fromString 1 1 input
                            |> flattenResult
                        )
                        expectedOutput
        , test "two-squared Grid" <|
            \_ ->
                let
                    expectedOutput =
                        Ok
                            [ LetterSquare ' '
                            , LetterSquare ' '
                            ]

                    input =
                        ".."
                in
                    Expect.equal
                        (Grid.fromString 2 1 input
                            |> flattenResult
                        )
                        expectedOutput
        , test "four-squared Grid" <|
            \_ ->
                let
                    expectedOutput =
                        Ok
                            [ LetterSquare ' '
                            , BlockSquare
                            , BlockSquare
                            , LetterSquare ' '
                            ]

                    input =
                        ".**."
                in
                    Expect.equal
                        (Grid.fromString 2 2 input
                            |> flattenResult
                        )
                        expectedOutput
        , test "invalid characters" <|
            \_ ->
                let
                    expectedOutput =
                        Err "Invalid character"

                    input =
                        "&$()"
                in
                    Expect.equal
                        (Grid.fromString 2 2 input
                            |> flattenResult
                        )
                        expectedOutput
        , test "too few characters" <|
            \_ ->
                let
                    expectedOutput =
                        Err "2 is too few characters for a 2x2 Grid"

                    input =
                        ".."
                in
                    Expect.equal
                        (Grid.fromString 2 2 input
                            |> flattenResult
                        )
                        expectedOutput
        , test "too many characters" <|
            \_ ->
                let
                    expectedOutput =
                        Err "10 is too many characters for a 3x3 Grid"

                    input =
                        ".........."
                in
                    Expect.equal
                        (Grid.fromString 3 3 input
                            |> flattenResult
                        )
                        expectedOutput
        ]
