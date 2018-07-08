module Views exposing (..)

import Puzzle exposing (Selection)
import Grid exposing (Grid)
import Square exposing (Square(..))
import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, style, value)
import Html.Events exposing (onClick, onInput, onFocus, onMouseDown)


gridView : Grid -> Maybe Selection -> (Coordinate -> msg) -> Html msg
gridView grid currentSelection clickMsg =
    let
        drawRow : List ( Coordinate, Square ) -> Html msg
        drawRow coordsWithSquares =
            div []
                (List.map (\( coord, sq ) -> squareView grid currentSelection clickMsg coord sq)
                    coordsWithSquares
                )
    in
        (div
            [ style
                [ ( "display", "inline-block" )
                , ( "border-left", "1px solid gray" )
                , ( "border-top", "1px solid gray" )
                , ( "font-family", "Helvetica, Arial, sans-serif" )
                , ( "user-select", "none" )
                ]
            ]
        )
            (grid
                |> Grid.toRows
                |> List.map (drawRow)
            )


squareView : Grid -> Maybe Selection -> (Coordinate -> msg) -> Coordinate -> Square -> Html msg
squareView grid currentSelection clickMsg (( x, y ) as coordinate) square =
    case square of
        LetterSquare _ letter _ ->
            let
                highlightStyle =
                    case currentSelection of
                        Just ( selectionCoordinate, direction ) ->
                            case direction of
                                Across ->
                                    if coordinate == selectionCoordinate then
                                        [ ( "background-color", "#009dff" ) ]
                                    else
                                        []

                                Down ->
                                    if coordinate == selectionCoordinate then
                                        [ ( "background-color", "#009dff" ) ]
                                    else
                                        []

                        Nothing ->
                            []
            in
                div
                    [ class "square--open"
                    , onMouseDown <| clickMsg ( x, y )
                    , style <|
                        [ ( "width", "32px" )
                        , ( "height", "32px" )
                        , ( "display", "inline-block" )
                        , ( "box-sizing", "border-box" )
                        , ( "vertical-align", "top" )
                        , ( "padding", "9px 0 0" )
                        , ( "font-size", "22px" )
                        , ( "text-align", "center" )
                        , ( "border-right", "1px solid gray" )
                        , ( "border-bottom", "1px solid gray" )
                        , ( "position", "relative" )
                        ]
                            ++ highlightStyle
                    ]
                    ([ text (String.fromChar letter)
                     ]
                    )

        BlockSquare _ ->
            div
                [ class "square--filled"
                , onMouseDown <| clickMsg ( x, y )
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
