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
    moveSelection Coordinate.atLeft permit puzzle


moveSelectionRight : SelectionPermit -> Puzzle -> Puzzle
moveSelectionRight permit puzzle =
    moveSelection Coordinate.atRight permit puzzle


moveSelectionUp : SelectionPermit -> Puzzle -> Puzzle
moveSelectionUp permit puzzle =
    moveSelection Coordinate.above permit puzzle


moveSelectionDown : SelectionPermit -> Puzzle -> Puzzle
moveSelectionDown permit puzzle =
    moveSelection Coordinate.below permit puzzle


moveSelection : (Coordinate -> Coordinate) -> SelectionPermit -> Puzzle -> Puzzle
moveSelection newCoordFn permit puzzle =
    let
        newSelection =
            case puzzle.currentSelection of
                Just coordinate ->
                    let
                        newCoord =
                            newCoordFn coordinate

                        trySquareAtNewCoord =
                            Result.map
                                (\g -> Grid.squareAtCoordinate g newCoord)
                                puzzle.grid
                    in
                        case trySquareAtNewCoord of
                            Err _ ->
                                puzzle.currentSelection

                            Ok maybeSquare ->
                                setSelection newCoord permit puzzle
                                    |> .currentSelection

                Nothing ->
                    Nothing
    in
        { puzzle | currentSelection = newSelection }


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
