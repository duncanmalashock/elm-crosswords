module PuzzleTests exposing (..)

import Puzzle exposing (EditMode(..))
import Grid
import Square exposing (Square(..))
import Direction exposing (Direction(..))
import Test exposing (Test, describe, test, skip)
import Expect


testSetSelection : Test
testSetSelection =
    describe "Puzzle.setSelection"
        [ describe "with all squares permitted for selection"
            [ test "sets a valid selection" <|
                \_ ->
                    let
                        newPuzzleSelectionCoordinate =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelectionCoordinate <| Ok (Just ( 0, 1 ))
            , test "doesn't set an invalid selection" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 6 ))
                    in
                        Expect.equal (Result.map .currentSelection newPuzzle) (Ok Nothing)
            ]
        , describe "with only letter squares permitted for selection"
            [ test "doesn't select a block square" <|
                \_ ->
                    let
                        newPuzzle =
                            Puzzle.fromString 2 2 "AB*D" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                    in
                        Expect.equal (Result.map .currentSelection newPuzzle) (Ok Nothing)
            ]
        ]


testSwitchSelectionDirection : Test
testSwitchSelectionDirection =
    describe "Puzzle.switchSelectionDirection"
        [ test "Switches from across to down" <|
            \_ ->
                let
                    newPuzzle =
                        Puzzle.fromString 2 2 "ABCD" Grid.blankClues Solving
                            |> Result.map (Puzzle.setSelection ( 0, 1 ))

                    input =
                        Result.map Puzzle.switchSelectionDirection newPuzzle
                            |> Result.map .currentSelection
                in
                    Expect.equal input (Ok (Just <| ( ( 0, 1 ), Down )))
        ]


testMoveSelectionLeft : Test
testMoveSelectionLeft =
    describe "Puzzle.moveSelectionLeft"
        [ describe "with all squares permitted for selection"
            [ test "moves left if there is a square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 1, 0 ))
                                |> Result.map Puzzle.moveSelectionLeft
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 0 ))
            , test "doesn't move left if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                                |> Result.map Puzzle.moveSelectionLeft
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 1 ))
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 2, 1 ))
                                |> Result.map Puzzle.moveSelectionLeft
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 1 ))
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
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 0 ))
                                |> Result.map Puzzle.moveSelectionRight
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 1, 0 ))
            , test "doesn't move right if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 1, 1 ))
                                |> Result.map Puzzle.moveSelectionRight
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 1, 1 ))
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                                |> Result.map Puzzle.moveSelectionRight
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 2, 1 ))
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
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                                |> Result.map Puzzle.moveSelectionUp
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 0 ))
            , test "doesn't move up if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 0 ))
                                |> Result.map Puzzle.moveSelectionUp
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 0 ))
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 1, 2 ))
                                |> Result.map Puzzle.moveSelectionUp
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 1, 0 ))
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
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 0 ))
                                |> Result.map Puzzle.moveSelectionDown
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 1 ))
            , test "doesn't move down if there is no square in bounds" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 2 2 "ABCD" Grid.blankClues Editing
                                |> Result.map (Puzzle.setSelection ( 0, 1 ))
                                |> Result.map Puzzle.moveSelectionDown
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 0, 1 ))
            ]
        , describe "with only letter squares permitted for selection"
            [ test "jumps over block squares to the next available letter square" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FGHI" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 1, 0 ))
                                |> Result.map Puzzle.moveSelectionDown
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 1, 2 ))
            , test "doesn't move if there are no valid squares left in the column" <|
                \_ ->
                    let
                        newPuzzleSelection =
                            Puzzle.fromString 3 3 "ABCD*FG*I" Grid.blankClues Solving
                                |> Result.map (Puzzle.setSelection ( 1, 0 ))
                                |> Result.map Puzzle.moveSelectionDown
                                |> Result.map Puzzle.selection
                                |> Result.map (Maybe.map Puzzle.selectionCoordinate)
                    in
                        Expect.equal newPuzzleSelection <| Ok (Just ( 1, 0 ))
            ]
        ]
