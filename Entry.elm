module Entry exposing (Entry)

import Coordinates exposing (Coordinates)


type Direction
    = Across
    | Down


type alias Entry =
    { startingCoordinates : Coordinates
    , direction : Direction
    , word : String
    , clue : String
    }
