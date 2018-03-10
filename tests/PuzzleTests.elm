module PuzzleTests exposing (..)

import Puzzle
import Grid exposing (Square(..))
import Test exposing (Test, describe, test, skip)
import Expect


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
        , test "invalid grid string" <|
            \_ ->
                let
                    stringInput =
                        "AB$D"

                    newPuzzle =
                        Puzzle.fromString 2 2 stringInput
                in
                    Expect.equal
                        newPuzzle.grid
                        (Err "Invalid character")
        , test "grid size mismatch (too few characters)" <|
            \_ ->
                let
                    stringInput =
                        "A"

                    newPuzzle =
                        Puzzle.fromString 2 2 stringInput
                in
                    Expect.equal
                        newPuzzle.grid
                        (Err "1 is too few characters for a 2x2 Grid")
        , test "grid size mismatch (too many characters)" <|
            \_ ->
                let
                    stringInput =
                        "ABCD"

                    newPuzzle =
                        Puzzle.fromString 1 1 stringInput
                in
                    Expect.equal
                        newPuzzle.grid
                        (Err "4 is too many characters for a 1x1 Grid")
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


testMoveSelectionLeft : Test
testMoveSelectionLeft =
    describe "Puzzle.moveSelectionLeft"
        [ test "moves left if there is a square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 1, 0 )
                            |> Puzzle.moveSelectionLeft
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
        , test "doesn't move left if there is no square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 1 )
                            |> Puzzle.moveSelectionLeft
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
        ]


testMoveSelectionRight : Test
testMoveSelectionRight =
    describe "Puzzle.moveSelectionRight"
        [ test "moves right if there is a square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 0 )
                            |> Puzzle.moveSelectionRight
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 1, 0 )
        , test "doesn't move right if there is no square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 1, 1 )
                            |> Puzzle.moveSelectionRight
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 1, 1 )
        ]


testMoveSelectionUp : Test
testMoveSelectionUp =
    describe "Puzzle.moveSelectionUp"
        [ test "moves up if there is a square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 1 )
                            |> Puzzle.moveSelectionUp
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
        , test "doesn't move up if there is no square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 0 )
                            |> Puzzle.moveSelectionUp
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
        ]


testMoveSelectionDown : Test
testMoveSelectionDown =
    describe "Puzzle.moveSelectionDown"
        [ test "moves down if there is a square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 0 )
                            |> Puzzle.moveSelectionDown
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
        , test "doesn't move down if there is no square in bounds" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD"
                            |> Puzzle.setSelection ( 0, 1 )
                            |> Puzzle.moveSelectionDown
                in
                    Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
        ]
