module Entry exposing (Entry, EntryStart(..), allFromGrid, view)

import Coordinate exposing (Coordinate)
import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type alias Entry =
    String


type EntryStart
    = AcrossOnly Int Entry
    | DownOnly Int Entry
    | AcrossAndDown Int Entry Entry


type alias EntryListings =
    List EntryStart


emptyEntryListings : EntryListings
emptyEntryListings =
    []


view : Grid -> Html msg
view grid =
    let
        acrossEntries =
            grid
                |> allFromGrid
                |> acrossList
                |> List.map entryView

        downEntries =
            grid
                |> allFromGrid
                |> downList
                |> List.map entryView
    in
        div
            [ style
                [ ( "display", "inline-block" )
                , ( "vertical-align", "top" )
                , ( "margin-left", "10px" )
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


acrossList : EntryListings -> List ( Int, Entry )
acrossList entryListings =
    entryListings
        |> List.map acrossFromEntryStart
        |> List.filter isJust
        |> List.map (Maybe.withDefault ( -1, "" ))


downList : EntryListings -> List ( Int, Entry )
downList entryListings =
    entryListings
        |> List.map downFromEntryStart
        |> List.filter isJust
        |> List.map (Maybe.withDefault ( -1, "" ))


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


acrossFromEntryStart : EntryStart -> Maybe ( Int, Entry )
acrossFromEntryStart entryStart =
    case entryStart of
        AcrossOnly int acrossEntry ->
            Just ( int, acrossEntry )

        DownOnly int downEntry ->
            Nothing

        AcrossAndDown int acrossEntry downEntry ->
            Just ( int, acrossEntry )


downFromEntryStart : EntryStart -> Maybe ( Int, Entry )
downFromEntryStart entryStart =
    case entryStart of
        AcrossOnly int acrossEntry ->
            Nothing

        DownOnly int downEntry ->
            Just ( int, downEntry )

        AcrossAndDown int acrossEntry downEntry ->
            Just ( int, downEntry )


allFromGrid : Grid -> EntryListings
allFromGrid grid =
    grid
        |> Matrix.toIndexedArray
        |> Array.foldl (atIndex grid) ( 1, [] )
        |> Tuple.second


atIndex : Grid -> ( ( Int, Int ), Square ) -> ( Int, List EntryStart ) -> ( Int, List EntryStart )
atIndex grid ( coord, square ) ( currentEntryNumber, entriesSoFar ) =
    case square of
        LetterSquare _ ->
            let
                newEntryStart =
                    if Grid.isAcrossEntryStart grid coord then
                        if Grid.isDownEntryStart grid coord then
                            [ AcrossAndDown currentEntryNumber (acrossEntry grid coord) (downEntry grid coord) ]
                        else
                            [ AcrossOnly currentEntryNumber (acrossEntry grid coord) ]
                    else if Grid.isDownEntryStart grid coord then
                        [ DownOnly currentEntryNumber (downEntry grid coord) ]
                    else
                        []

                nextEntryNumber =
                    if List.isEmpty newEntryStart then
                        currentEntryNumber
                    else
                        currentEntryNumber + 1
            in
                ( nextEntryNumber, entriesSoFar ++ newEntryStart )

        BlockSquare ->
            ( currentEntryNumber, entriesSoFar )


acrossEntry : Grid -> Coordinate -> String
acrossEntry grid coordinate =
    acrossEntryHelp grid coordinate ""
        |> String.reverse


acrossEntryHelp : Grid -> Coordinate -> String -> String
acrossEntryHelp grid coordinate entrySoFar =
    case Grid.squareAtCoordinate grid coordinate of
        Just (LetterSquare char) ->
            acrossEntryHelp grid (Coordinate.atRight coordinate) (String.cons char entrySoFar)

        _ ->
            entrySoFar


downEntry : Grid -> Coordinate -> String
downEntry grid coordinate =
    downEntryHelp grid coordinate ""
        |> String.reverse


downEntryHelp : Grid -> Coordinate -> String -> String
downEntryHelp grid coordinate entrySoFar =
    case Grid.squareAtCoordinate grid coordinate of
        Just (LetterSquare char) ->
            downEntryHelp grid (Coordinate.below coordinate) (String.cons char entrySoFar)

        _ ->
            entrySoFar
