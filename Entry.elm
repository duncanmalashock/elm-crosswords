module Entry exposing (Entry, Direction(..))

import Coordinates exposing (Coordinates)


type Direction
    = Across
    | Down


type alias Entry =
    { startingCoordinates : Coordinates
    , direction : Direction
    , length : Int
    }
