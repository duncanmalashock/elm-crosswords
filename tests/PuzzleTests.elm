module PuzzleTests exposing (..)

import Puzzle exposing (SelectionPermit(..))
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
        [ describe "with all squares permitted for selection"
            [ test "sets a valid selection" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
            , test "doesn't set an invalid selection" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 6 ) CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection Nothing
            ]
        , describe "with only letter squares permitted for selection"
            [ test "doesn't select a block square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "AB*D"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectOnlyLetterSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Nothing
            ]
        ]


testMoveSelectionLeft : Test
testMoveSelectionLeft =
    describe "Puzzle.moveSelectionLeft"
        [ describe "with all squares permitted for selection"
            [ test "moves left if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 1, 0 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionLeft CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
            , test "doesn't move left if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionLeft CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 3 3 "ABCD*FGHI"
                                |> Puzzle.setSelection ( 2, 1 ) CanSelectOnlyLetterSquares
                                |> Puzzle.moveSelectionLeft CanSelectOnlyLetterSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
            ]
        ]


testMoveSelectionRight : Test
testMoveSelectionRight =
    describe "Puzzle.moveSelectionRight"
        [ describe "with all squares permitted for selection"
            [ test "moves right if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 0 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionRight CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 1, 0 )
            , test "doesn't move right if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 1, 1 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionRight CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 1, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 3 3 "ABCD*FGHI"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectOnlyLetterSquares
                                |> Puzzle.moveSelectionRight CanSelectOnlyLetterSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 2, 1 )
            ]
        ]


testMoveSelectionUp : Test
testMoveSelectionUp =
    describe "Puzzle.moveSelectionUp"
        [ describe "with all squares permitted for selection"
            [ test "moves up if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionUp CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
            , test "doesn't move up if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 0 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionUp CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 0 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 3 3 "ABCD*FGHI"
                                |> Puzzle.setSelection ( 1, 2 ) CanSelectOnlyLetterSquares
                                |> Puzzle.moveSelectionUp CanSelectOnlyLetterSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 1, 0 )
            ]
        ]


testMoveSelectionDown : Test
testMoveSelectionDown =
    describe "Puzzle.moveSelectionDown"
        [ describe "with all squares permitted for selection"
            [ test "moves down if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 0 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionDown CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
            , test "doesn't move down if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD"
                                |> Puzzle.setSelection ( 0, 1 ) CanSelectAllSquares
                                |> Puzzle.moveSelectionDown CanSelectAllSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 0, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 3 3 "ABCD*FGHI"
                                |> Puzzle.setSelection ( 1, 0 ) CanSelectOnlyLetterSquares
                                |> Puzzle.moveSelectionDown CanSelectOnlyLetterSquares
                    in
                        Expect.equal newPuzzle.currentSelection <| Just ( 1, 2 )
            ]
        ]
