module Views exposing (..)

import Puzzle exposing (Selection)
import Grid exposing (Grid, Square(..))
import Clue exposing (ClueDict, Clue)
import Entry exposing (EntryStartDict, EntryMembershipDict)
import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


gridView : Grid -> Maybe Selection -> EntryStartDict -> EntryMembershipDict -> (Coordinate -> msg) -> Html msg
gridView grid currentSelection entryListings entryMembershipDict clickMsg =
    let
        drawRow : List ( Coordinate, Square ) -> Html msg
        drawRow coordsWithSquares =
            div []
                (List.map (\( coord, sq ) -> squareView grid currentSelection entryListings entryMembershipDict clickMsg coord sq)
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


squareView : Grid -> Maybe Selection -> EntryStartDict -> EntryMembershipDict -> (Coordinate -> msg) -> Coordinate -> Square -> Html msg
squareView grid currentSelection entryListings entryMembershipDict clickMsg (( x, y ) as coordinate) square =
    case square of
        LetterSquare letter ->
            let
                highlightStyle =
                    case currentSelection of
                        Just ( selectionCoordinate, direction ) ->
                            case direction of
                                Across ->
                                    let
                                        selectionEntry =
                                            Entry.acrossEntryMembership selectionCoordinate entryMembershipDict
                                    in
                                        if coordinate == selectionCoordinate then
                                            [ ( "background-color", "#009dff" ) ]
                                        else if (Entry.acrossEntryMembership coordinate entryMembershipDict == selectionEntry) then
                                            [ ( "background-color", "#e5f5ff" ) ]
                                        else
                                            []

                                Down ->
                                    let
                                        selectionEntry =
                                            Entry.downEntryMembership selectionCoordinate entryMembershipDict
                                    in
                                        if coordinate == selectionCoordinate then
                                            [ ( "background-color", "#009dff" ) ]
                                        else if (Entry.downEntryMembership coordinate entryMembershipDict == selectionEntry) then
                                            [ ( "background-color", "#e5f5ff" ) ]
                                        else
                                            []

                        Nothing ->
                            []

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
                        ++ entryStartView
                    )

        BlockSquare ->
            div
                [ class "square--filled"
                , onClick <| clickMsg ( x, y )
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


cluesView : ClueDict -> Html msg
cluesView clues =
    let
        acrossClues =
            clues
                |> Clue.across
                |> List.map clueView

        downClues =
            clues
                |> Clue.down
                |> List.map clueView
    in
        div
            [ style
                [ ( "display", "inline-block" )
                , ( "vertical-align", "top" )
                , ( "margin-left", "10px" )
                , ( "font-family", "Helvetica, Arial, sans-serif" )
                , ( "height", "481px" )
                , ( "overflow", "auto" )
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
                    :: acrossClues
                )
            , div
                [ style
                    [ ( "display", "inline-block" )
                    , ( "vertical-align", "top" )
                    , ( "margin-left", "10px" )
                    ]
                ]
                ((div [] [ text "Down" ])
                    :: downClues
                )
            ]


clueView : Clue -> Html msg
clueView clue =
    div []
        [ text <| (toString clue.index) ++ ": " ++ clue.clue ]
