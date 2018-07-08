module PuzzleTests exposing (..)

import Puzzle exposing (EditMode(..))
import Square exposing (Square(..))
import Direction exposing (Direction(..))
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
                        Puzzle.fromString 2 2 stringInput Editing
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
                        Puzzle.fromString 2 2 stringInput Editing
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
                        Puzzle.fromString 2 2 stringInput Editing
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
                        Puzzle.fromString 1 1 stringInput Editing
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
                        newPuzzleSelectionCoordinate =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 1 )
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelectionCoordinate <| Just ( 0, 1 )
            , test "doesn't set an invalid selection" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 6 )
                    in
                        Expect.equal newPuzzle.currentSelection Nothing
            ]
        , describe "with only letter squares permitted for selection"
            [ test "doesn't select a block square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "AB*D" Solving
                                |> Puzzle.setSelection ( 0, 1 )
                    in
                        Expect.equal newPuzzle.currentSelection <| Nothing
            ]
        ]


testSwitchSelectionDirection : Test
testSwitchSelectionDirection =
    describe "Puzzle.switchSelectionDirection"
        [ test "Switches from across to down" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD" Solving
                            |> Puzzle.setSelection ( 0, 1 )

                    input =
                        Puzzle.switchSelectionDirection newPuzzle
                            |> .currentSelection
                in
                    Expect.equal input (Just <| ( ( 0, 1 ), Down ))
        ]


testMoveSelectionLeft : Test
testMoveSelectionLeft =
    describe "Puzzle.moveSelectionLeft"
        [ describe "with all squares permitted for selection"
            [ test "moves left if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 1, 0 )
                                |> Puzzle.moveSelectionLeft
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 0 )
            , test "doesn't move left if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 1 )
                                |> Puzzle.moveSelectionLeft
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Solving
                                |> Puzzle.setSelection ( 2, 1 )
                                |> Puzzle.moveSelectionLeft
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 1 )
            ]
        ]


testMoveSelectionRight : Test
testMoveSelectionRight =
    describe "Puzzle.moveSelectionRight"
        [ describe "with all squares permitted for selection"
            [ test "moves right if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 0 )
                                |> Puzzle.moveSelectionRight
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 1, 0 )
            , test "doesn't move right if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 1, 1 )
                                |> Puzzle.moveSelectionRight
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 1, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Solving
                                |> Puzzle.setSelection ( 0, 1 )
                                |> Puzzle.moveSelectionRight
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 2, 1 )
            ]
        ]


testMoveSelectionUp : Test
testMoveSelectionUp =
    describe "Puzzle.moveSelectionUp"
        [ describe "with all squares permitted for selection"
            [ test "moves up if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 1 )
                                |> Puzzle.moveSelectionUp
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 0 )
            , test "doesn't move up if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 0 )
                                |> Puzzle.moveSelectionUp
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 0 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Solving
                                |> Puzzle.setSelection ( 1, 2 )
                                |> Puzzle.moveSelectionUp
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 1, 0 )
            ]
        ]


testMoveSelectionDown : Test
testMoveSelectionDown =
    describe "Puzzle.moveSelectionDown"
        [ describe "with all squares permitted for selection"
            [ test "moves down if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 0 )
                                |> Puzzle.moveSelectionDown
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 1 )
            , test "doesn't move down if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Editing
                                |> Puzzle.setSelection ( 0, 1 )
                                |> Puzzle.moveSelectionDown
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 0, 1 )
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Solving
                                |> Puzzle.setSelection ( 1, 0 )
                                |> Puzzle.moveSelectionDown
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 1, 2 )
            , test "doesn't move if there are no valid squares left in the column" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FG*I" Solving
                                |> Puzzle.setSelection ( 1, 0 )
                                |> Puzzle.moveSelectionDown
                                |> Puzzle.selection
                                |> Maybe.map Puzzle.selectionCoordinate
                    in
                        Expect.equal newPuzzleSelection <| Just ( 1, 0 )
            ]
        ]
