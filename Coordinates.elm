module Coordinates
    exposing
        ( Coordinates
        , xCoordinate
        , yCoordinate
        , above
        , atLeft
        )


type alias Coordinates =
    ( Int, Int )


xCoordinate : Coordinates -> Int
xCoordinate ( x, _ ) =
    x


yCoordinate : Coordinates -> Int
yCoordinate ( _, y ) =
    y


above : Coordinates -> Coordinates
above ( x, y ) =
    ( x, y - 1 )


atLeft : Coordinates -> Coordinates
atLeft ( x, y ) =
    ( x - 1, y )
