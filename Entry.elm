module Entry exposing (Entry, Direction(..))

import Coordinate exposing (Coordinate)


type Direction
    = Across
    | Down


type alias Entry =
    { startingCoordinate : Coordinate
    , direction : Direction
    , length : Int
    , value : List Char
    , clue : String
    }
