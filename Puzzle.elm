module Puzzle exposing (Puzzle, fromString, setSelection)

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
