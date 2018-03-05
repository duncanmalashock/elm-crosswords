module Views exposing (..)

import Grid exposing (Grid, Square(..))
import Entry exposing (EntryListings)
import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


gridView : Grid -> EntryListings -> (Coordinate -> msg) -> Html msg
gridView grid entryListings clickMsg =
    let
        drawRow : List ( Coordinate, Square ) -> Html msg
        drawRow coordsWithSquares =
            div []
                (List.map (\( coord, sq ) -> squareView grid entryListings clickMsg coord sq)
                    coordsWithSquares
                )
    in
        (div
            [ style
                [ ( "display", "inline-block" )
                , ( "border-left", "1px solid gray" )
                , ( "border-top", "1px solid gray" )
                , ( "font-family", "Helvetica, Arial, sans-serif" )
                ]
            ]
        )
            (grid
                |> Grid.toRows
                |> List.map (drawRow)
            )


squareView : Grid -> EntryListings -> (Coordinate -> msg) -> Coordinate -> Square -> Html msg
squareView grid entryListings clickMsg ( x, y ) square =
    case square of
        LetterSquare letter ->
            let
                entryStartView =
                    case Entry.entryNumberAt entryListings ( x, y ) of
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
                    , onClick <| clickMsg ( x, y )
                    , style
                        [ ( "width", "32px" )
                        , ( "height", "32px" )
                        , ( "display", "inline-block" )
                        , ( "box-sizing", "border-box" )
                        , ( "vertical-align", "top" )
                        , ( "padding", "10px 0 0" )
                        , ( "font-size", "21px" )
                        , ( "text-align", "center" )
                        , ( "border-right", "1px solid gray" )
                        , ( "border-bottom", "1px solid gray" )
                        , ( "position", "relative" )
                        ]
                    ]
                    ([ text (String.fromChar letter)
                     ]
                        ++ entryStartView
                    )

        BlockSquare ->
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


entriesView : EntryListings -> Html msg
entriesView entryListings =
    let
        acrossEntries =
            entryListings
                |> Entry.acrossList
                |> List.map entryView

        downEntries =
            entryListings
                |> Entry.downList
                |> List.map entryView
    in
        div
            [ style
                [ ( "display", "inline-block" )
                , ( "vertical-align", "top" )
                , ( "margin-left", "10px" )
                , ( "font-family", "Helvetica, Arial, sans-serif" )
                ]
            ]
            [ div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "vertical-align", "top" )
                    , ( "margin-left", "10px" )
                    ]
                ]
                ((div [] [ text "Across" ])
                    :: acrossEntries
                )
            , div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "vertical-align", "top" )
                    , ( "margin-left", "10px" )
                    ]
                ]
                ((div [] [ text "Down" ])
                    :: downEntries
                )
            ]


entryView : ( Int, String ) -> Html msg
entryView ( int, entry ) =
    div []
        [ text <| (toString int) ++ ": " ++ entry ]
