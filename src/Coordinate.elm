module Coordinate
    exposing
        ( Coordinate
        , above
        , below
        , atLeft
        , atRight
        )


type alias Coordinate =
    ( Int, Int )


above : Coordinate -> Coordinate
above ( x, y ) =
    ( x, y - 1 )


below : Coordinate -> Coordinate
below ( x, y ) =
    ( x, y + 1 )


atLeft : Coordinate -> Coordinate
atLeft ( x, y ) =
    ( x - 1, y )


atRight : Coordinate -> Coordinate
atRight ( x, y ) =
    ( x + 1, y )
