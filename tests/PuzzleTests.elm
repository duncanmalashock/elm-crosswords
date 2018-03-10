module PuzzleTests exposing (..)

import Puzzle
import Grid exposing (Square(..))
import Test exposing (Test, describe, test, skip)
import Expect


-- TODO: tests for:
-- Puzzle.moveSelectionLeft
-- Puzzle.moveSelectionRight
-- Puzzle.moveSelectionUp
-- Puzzle.moveSelectionDown


testFromString : Test
testFromString =
    describe "Puzzle.fromString"
        [ test "valid grid string" <|
            \_ ->
                let
                    stringInput =
                        "ABCD"

                    newPuzzle =
                        Puzzle.fromString 2 2 stringInput
                in
                    Expect.equal
                        (newPuzzle.grid
                            |> Result.map (Grid.flatten)
                        )
                        (Ok
                            [ LetterSquare 'A'
                            , LetterSquare 'B'
                            , LetterSquare 'C'
                            , LetterSquare 'D'
                            ]
                        )
        ]


testSetSelection : Test
testSetSelection =
    describe "Puzzle.setSelection"
        [ test "sets a valid selection" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 1 )
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
        , test "doesn't set an invalid selection" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 6 )
                in
                    Expect.equal newPuzzle.currentSelection Nothing
        ]
