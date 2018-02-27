module Puzzle exposing (Puzzle)

import Grid exposing (Grid)
import Entry exposing (Entry, Direction(..))
import Html exposing (Html, div)


type alias Puzzle =
    { entries : List Entry
    , grid : Grid
    }
