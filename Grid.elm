module Grid exposing (Grid, initGrid, view)

import Coordinates exposing (Coordinates)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra


type alias Grid =
    List Square


type alias Letter =
    Char


type Square
    = White Coordinates Letter
    | Black Coordinates


initGrid : Grid
initGrid =
    createGridWithRotationalSymmetry
        [ [ 6, 1, 3, 1, 4 ]
        , [ 6, 1, 3, 1, 4 ]
        , [ 6, 1, 8 ]
        , [ 4, 1, 3, 2, 5 ]
        , [ 3, 1, 7, 1, 3 ]
        , [ 7, 1, 6, 1, 0 ]
        , [ 5, 2, 5, 3, 0 ]
        , [ 0, 1, 3, 1, 5, 1, 3, 1, 0 ]
        ]


gridToRows : Grid -> List (List Square)
gridToRows grid =
    let
        hasSameY square1 square2 =
            squareYCoordinate square1 == squareYCoordinate square2
    in
        grid
            |> List.sortBy (squareCoordinates >> Coordinates.yCoordinate)
            |> List.Extra.groupWhile hasSameY


view : Grid -> Html msg
view grid =
    let
        sortRow : List Square -> List Square
        sortRow squares =
            List.sortBy (\s -> squareXCoordinate s) squares

        drawRow : List Square -> Html msg
        drawRow squares =
            div []
                (List.map (squareView) squares)
    in
        (div
            [ style
                [ ( "display", "inline-block" )
                , ( "border-left", "1px solid gray" )
                , ( "border-top", "1px solid gray" )
                ]
            ]
        )
            (grid
                |> gridToRows
                |> List.map (sortRow >> drawRow)
            )


squareView : Square -> Html msg
squareView square =
    case square of
        White coords letter ->
            div
                [ class "square--open"
                , style
                    [ ( "width", "32px" )
                    , ( "height", "32px" )
                    , ( "display", "inline-block" )
                    , ( "box-sizing", "border-box" )
                    , ( "vertical-align", "top" )
                    , ( "padding", "8px 0" )
                    , ( "text-align", "center" )
                    , ( "border-right", "1px solid gray" )
                    , ( "border-bottom", "1px solid gray" )
                    ]
                ]
                [ text <| String.fromChar letter ]

        Black coords ->
            div
                [ class "square--filled"
                , style
                    [ ( "width", "32px" )
                    , ( "height", "32px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "black" )
                    , ( "box-sizing", "border-box" )
                    , ( "vertical-align", "top" )
                    ]
                ]
                []


createGridWithRotationalSymmetry : List (List Int) -> Grid
createGridWithRotationalSymmetry gridSpec =
    let
        flipGridSpec spec =
            spec
                |> List.map (List.reverse)
                |> List.reverse
                |> List.drop 1
    in
        doCreateGrid (gridSpec ++ flipGridSpec gridSpec)


doCreateGrid : List (List Int) -> Grid
doCreateGrid gridSpec =
    recursiveCreateGrid gridSpec [] 0
        |> List.concat
        |> List.concat


recursiveCreateGrid : List (List Int) -> Grid -> Int -> List (List Grid)
recursiveCreateGrid lists grid index =
    case lists of
        [] ->
            []

        intList :: intLists ->
            gridRow intList [] ( 0, index ) :: (recursiveCreateGrid intLists grid (index + 1))


gridRow : List Int -> Grid -> Coordinates -> List Grid
gridRow squareLists grid ( x, y ) =
    case squareLists of
        [] ->
            []

        int :: ints ->
            if List.length ints % 2 == 0 then
                blankSquares ( x, y ) int :: gridRow ints grid ( x + int, y )
            else
                filledSquares ( x, y ) int :: gridRow ints grid ( x + int, y )


blankSquare : Coordinates -> Square
blankSquare coordinates =
    White coordinates ' '


filledSquare : Coordinates -> Square
filledSquare coordinates =
    Black coordinates


blankSquares : Coordinates -> Int -> List Square
blankSquares ( x, y ) number =
    if number <= 0 then
        []
    else
        blankSquare ( x, y ) :: blankSquares ( x + 1, y ) (number - 1)


filledSquares : Coordinates -> Int -> List Square
filledSquares ( x, y ) number =
    if number <= 0 then
        []
    else
        filledSquare ( x, y ) :: filledSquares ( x + 1, y ) (number - 1)


squareCoordinates : Square -> Coordinates
squareCoordinates square =
    case square of
        White coords _ ->
            coords

        Black coords ->
            coords


squareXCoordinate : Square -> Int
squareXCoordinate square =
    Coordinates.xCoordinate <| squareCoordinates square


squareYCoordinate : Square -> Int
squareYCoordinate square =
    Coordinates.yCoordinate <| squareCoordinates square
