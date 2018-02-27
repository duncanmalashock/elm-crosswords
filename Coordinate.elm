module Coordinate
    exposing
        ( Coordinate
        , xCoordinate
        , yCoordinate
        , above
        , atLeft
        )


type alias Coordinate =
    ( Int, Int )


xCoordinate : Coordinate -> Int
xCoordinate ( x, _ ) =
    x


yCoordinate : Coordinate -> Int
yCoordinate ( _, y ) =
    y


above : Coordinate -> Coordinate
above ( x, y ) =
    ( x, y - 1 )


atLeft : Coordinate -> Coordinate
atLeft ( x, y ) =
    ( x - 1, y )
