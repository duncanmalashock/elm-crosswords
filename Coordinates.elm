module Coordinates exposing (Coordinates, xCoordinate, yCoordinate)


type alias Coordinates =
    ( Int, Int )


xCoordinate : Coordinates -> Int
xCoordinate ( x, _ ) =
    x


yCoordinate : Coordinates -> Int
yCoordinate ( _, y ) =
    y
