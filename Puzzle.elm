module Puzzle
    exposing
        ( Puzzle
        , SelectionPermit(..)
        , fromString
        , setSelection
        , moveSelectionLeft
        , moveSelectionRight
        , moveSelectionUp
        , moveSelectionDown
        )

import Grid exposing (Grid)
import Coordinate exposing (Coordinate)


type alias Puzzle =
    { grid : Result String Grid
    , currentSelection : Maybe Coordinate
    }


type SelectionPermit
    = CanSelectAllSquares
    | CanSelectOnlyLetterSquares


fromString : Int -> Int -> String -> Puzzle
fromString gridWidth gridHeight string =
    { grid = Grid.fromString gridWidth gridHeight string
    , currentSelection = Nothing
    }


moveSelectionLeft : SelectionPermit -> Puzzle -> Puzzle
moveSelectionLeft permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.atLeft permit puzzle


moveSelectionRight : SelectionPermit -> Puzzle -> Puzzle
moveSelectionRight permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.atRight permit puzzle


moveSelectionUp : SelectionPermit -> Puzzle -> Puzzle
moveSelectionUp permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.above permit puzzle


moveSelectionDown : SelectionPermit -> Puzzle -> Puzzle
moveSelectionDown permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.below permit puzzle


moveSelection : Maybe Coordinate -> (Coordinate -> Coordinate) -> SelectionPermit -> Puzzle -> Puzzle
moveSelection startingSelection newCoordFn permit puzzle =
    case puzzle.currentSelection of
        Just coordinate ->
            let
                newCoordToTry =
                    newCoordFn coordinate

                newCoordIsInBounds grid =
                    Grid.coordIsInBounds grid newCoordToTry

                newSquareIsPermitted grid =
                    (Grid.hasLetterSquareAt grid newCoordToTry)
                        || (permit == CanSelectAllSquares)
            in
                case (Result.map newCoordIsInBounds puzzle.grid) of
                    Ok True ->
                        case (Result.map newSquareIsPermitted puzzle.grid) of
                            Ok True ->
                                setSelection newCoordToTry permit puzzle

                            Ok False ->
                                moveSelection
                                    startingSelection
                                    newCoordFn
                                    permit
                                    { puzzle
                                        | currentSelection =
                                            Maybe.map (\s -> newCoordFn s)
                                                puzzle.currentSelection
                                    }

                            Err _ ->
                                puzzle

                    Ok False ->
                        { puzzle | currentSelection = startingSelection }

                    Err _ ->
                        puzzle

        Nothing ->
            puzzle


setSelection : Coordinate -> SelectionPermit -> Puzzle -> Puzzle
setSelection (( x, y ) as coordinate) permit puzzle =
    case puzzle.grid of
        Ok grid ->
            let
                isInXBounds xVal =
                    xVal >= 0 && xVal < (Grid.width grid)

                isInYBounds yVal =
                    yVal >= 0 && yVal < (Grid.height grid)

                squareIsPermitted =
                    (Grid.hasLetterSquareAt grid coordinate)
                        || (permit == CanSelectAllSquares)
            in
                if (isInXBounds x && isInYBounds y && squareIsPermitted) then
                    { puzzle | currentSelection = Just coordinate }
                else
                    puzzle

        Err _ ->
            { puzzle | currentSelection = Nothing }
