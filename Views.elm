module Views exposing (..)

import Grid exposing (Grid, Square(..))
import Entry exposing (Entry, EntryListings)
import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


gridView : Grid -> (Coordinate -> msg) -> Html msg
gridView grid clickMsg =
    let
        drawRow : List Square -> Html msg
        drawRow squares =
            div []
                (List.map (squareView grid clickMsg) squares)
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


squareView : Grid -> (Coordinate -> msg) -> Square -> Html msg
squareView grid clickMsg square =
    case square of
        LetterSquare letter ->
            div
                [ class "square--open"
                , onClick <| clickMsg ( 2, 2 )
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
                [ text (String.fromChar letter) ]

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


entryView : ( Int, Entry ) -> Html msg
entryView ( int, entry ) =
    div []
        [ text <| (toString int) ++ ": " ++ entry ]
