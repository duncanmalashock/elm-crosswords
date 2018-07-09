module Views exposing (..)

import Puzzle exposing (Selection)
import Grid exposing (Grid)
import Square exposing (Square(..))
import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Maybe.Extra as Maybe
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
        LetterSquare _ letterData _ ->
            let
                highlightStyle =
                    case currentSelection of
                        Just ( selectionCoordinate, direction ) ->
                            let
                                selectedSquare =
                                    Grid.squareAtCoordinate grid selectionCoordinate

                                selectedClue =
                                    case direction of
                                        Across ->
                                            Maybe.map Square.acrossEntryNumber selectedSquare
                                                |> Maybe.join
                                                |> Maybe.withDefault -1

                                        Down ->
                                            Maybe.map Square.downEntryNumber selectedSquare
                                                |> Maybe.join
                                                |> Maybe.withDefault -1
                            in
                                if coordinate == selectionCoordinate then
                                    [ ( "background-color", "#009dff" ) ]
                                else if Square.isInEntry selectedClue direction square then
                                    [ ( "background-color", "#ccebff" ) ]
                                else
                                    []

                        Nothing ->
                            []

                entryNumberView =
                    case Square.entryNumber square of
                        Just i ->
                            [ div
                                [ style
                                    [ ( "position", "absolute" )
                                    , ( "top", "0px" )
                                    , ( "left", "2px" )
                                    , ( "font-size", "10px" )
                                    ]
                                ]
                                [ text <| toString i ]
                            ]

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
                    ([ text (String.fromChar letterData.guess)
                     ]
                        ++ entryNumberView
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
