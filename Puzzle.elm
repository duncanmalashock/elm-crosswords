module Puzzle
    exposing
        ( Puzzle
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


fromString : Int -> Int -> String -> Puzzle
fromString gridWidth gridHeight string =
    { grid = Grid.fromString gridWidth gridHeight string
    , currentSelection = Nothing
    }


moveSelectionLeft : Puzzle -> Puzzle
moveSelectionLeft puzzle =
    moveSelection Coordinate.atLeft puzzle


moveSelectionRight : Puzzle -> Puzzle
moveSelectionRight puzzle =
    moveSelection Coordinate.atRight puzzle


moveSelectionUp : Puzzle -> Puzzle
moveSelectionUp puzzle =
    moveSelection Coordinate.above puzzle


moveSelectionDown : Puzzle -> Puzzle
moveSelectionDown puzzle =
    moveSelection Coordinate.below puzzle


moveSelection : (Coordinate -> Coordinate) -> Puzzle -> Puzzle
moveSelection newCoordFn puzzle =
    let
        newSelection =
            case puzzle.currentSelection of
                Just coordinate ->
                    let
                        squareAtNewCoord =
                            newCoordFn coordinate

                        trySquareAtNewCoord =
                            Result.map (\g -> Grid.squareAtCoordinate g squareAtNewCoord) puzzle.grid
                                |> Result.toMaybe
                    in
                        case trySquareAtNewCoord of
                            Nothing ->
                                puzzle.currentSelection

                            Just maybeSquare ->
                                case maybeSquare of
                                    Nothing ->
                                        puzzle.currentSelection

                                    Just _ ->
                                        Just squareAtNewCoord

                Nothing ->
                    Nothing
    in
        { puzzle | currentSelection = newSelection }


setSelection : Coordinate -> Puzzle -> Puzzle
setSelection (( x, y ) as coordinate) puzzle =
    case puzzle.grid of
        Ok grid ->
            let
                isInXBounds xVal =
                    xVal >= 0 && xVal < (Grid.width grid)

                isInYBounds yVal =
                    yVal >= 0 && yVal < (Grid.height grid)
            in
                if (isInXBounds x && isInYBounds y) then
                    { puzzle | currentSelection = Just coordinate }
                else
                    { puzzle | currentSelection = Nothing }

        Err _ ->
            { puzzle | currentSelection = Nothing }
