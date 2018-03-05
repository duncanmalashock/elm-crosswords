module Entry
    exposing
        ( EntryStart(..)
        , EntryListings
        , allFromGrid
        , entryNumberAt
        , acrossList
        , downList
        )

import Coordinate exposing (Coordinate)
import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type alias Entry =
    ( Int, String )


type EntryStart
    = AcrossOnly Int String
    | DownOnly Int String
    | AcrossAndDown Int String String


type alias EntryListings =
    Dict Coordinate EntryStart


emptyEntryListings : EntryListings
emptyEntryListings =
    Dict.empty


entryNumberAt : EntryListings -> Coordinate -> Maybe Int
entryNumberAt entryListings coordinate =
    let
        getEntryNumber entryStart =
            case entryStart of
                AcrossOnly i _ ->
                    i

                DownOnly i _ ->
                    i

                AcrossAndDown i _ _ ->
                    i
    in
        Dict.get coordinate entryListings
            |> Maybe.map getEntryNumber


acrossList : EntryListings -> List Entry
acrossList entryListings =
    entryListings
        |> Dict.values
        |> List.map acrossFromEntryStart
        |> List.filter isJust
        |> List.map (Maybe.withDefault ( -1, "" ))
        |> sortEntries


downList : EntryListings -> List Entry
downList entryListings =
    entryListings
        |> Dict.values
        |> List.map downFromEntryStart
        |> List.filter isJust
        |> List.map (Maybe.withDefault ( -1, "" ))
        |> sortEntries


sortEntries : List Entry -> List Entry
sortEntries =
    List.sortBy (\( i, e ) -> i)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


acrossFromEntryStart : EntryStart -> Maybe Entry
acrossFromEntryStart entryStart =
    case entryStart of
        AcrossOnly int acrossEntry ->
            Just ( int, acrossEntry )

        DownOnly int downEntry ->
            Nothing

        AcrossAndDown int acrossEntry downEntry ->
            Just ( int, acrossEntry )


downFromEntryStart : EntryStart -> Maybe Entry
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
        |> Array.foldl (updateFromCoordinate grid) ( 1, Dict.empty )
        |> Tuple.second


updateFromCoordinate : Grid -> ( ( Int, Int ), Square ) -> ( Int, EntryListings ) -> ( Int, EntryListings )
updateFromCoordinate grid ( coord, square ) ( currentEntryNumber, entriesSoFar ) =
    case square of
        LetterSquare _ ->
            let
                createNewEntryStart =
                    Grid.isAcrossEntryStart grid coord
                        || Grid.isDownEntryStart grid coord

                nextEntryNumber =
                    if createNewEntryStart then
                        currentEntryNumber + 1
                    else
                        currentEntryNumber
            in
                if createNewEntryStart then
                    let
                        newEntryStart =
                            if Grid.isAcrossEntryStart grid coord then
                                if Grid.isDownEntryStart grid coord then
                                    AcrossAndDown currentEntryNumber
                                        (acrossEntry grid coord)
                                        (downEntry grid coord)
                                else
                                    AcrossOnly currentEntryNumber
                                        (acrossEntry grid coord)
                            else
                                DownOnly currentEntryNumber
                                    (downEntry grid coord)
                    in
                        ( nextEntryNumber, Dict.insert coord newEntryStart entriesSoFar )
                else
                    ( currentEntryNumber, entriesSoFar )

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
            acrossEntryHelp grid
                (Coordinate.atRight coordinate)
                (String.cons char entrySoFar)

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
            downEntryHelp grid
                (Coordinate.below coordinate)
                (String.cons char entrySoFar)

        _ ->
            entrySoFar
