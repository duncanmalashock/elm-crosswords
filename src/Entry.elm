module Entry
    exposing
        ( Entry
        , entry
        , EntryStartDict
        , EntryStart(..)
        , EntryMembershipDict
        , updateEntry
        , entryStartDictFromGrid
        , entryNumberAt
        , entryMembershipDictFromEntryStartDict
        , acrossEntryMembership
        , downEntryMembership
        , flattenEntryMembershipDict
        , acrossList
        , downList
        )

import Direction exposing (Direction(..))
import Coordinate exposing (Coordinate)
import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)


type alias Entry =
    { number : Int
    , direction : Direction
    , text : String
    , clue : String
    }


entry : Int -> Direction -> String -> String -> Entry
entry number direction text clue =
    { number = number
    , direction = direction
    , text = text
    , clue = clue
    }


type EntryStart
    = AcrossOnlyStart Entry
    | DownOnlyStart Entry
    | AcrossAndDownStarts Entry Entry


type alias EntryStartDict =
    Dict Coordinate EntryStart


type EntryMembership
    = BelongsToNoEntries
    | BelongsToEntries { across : Int, down : Int }


type alias EntryMembershipDict =
    Dict Coordinate EntryMembership


entryNumberAt : EntryStartDict -> Coordinate -> Maybe Int
entryNumberAt entryListings coordinate =
    let
        getEntryNumber entryStart =
            case entryStart of
                AcrossOnlyStart a ->
                    a.number

                DownOnlyStart d ->
                    d.number

                AcrossAndDownStarts a d ->
                    a.number
    in
        Dict.get coordinate entryListings
            |> Maybe.map getEntryNumber


updateEntry : EntryStartDict -> Coordinate -> Entry -> EntryStartDict
updateEntry entryStartDict coordinate newEntry =
    let
        updateMaybeEntryStart =
            Maybe.map
                (\es ->
                    case es of
                        AcrossOnlyStart acrossEntry ->
                            AcrossOnlyStart newEntry

                        DownOnlyStart downEntry ->
                            DownOnlyStart newEntry

                        AcrossAndDownStarts acrossEntry downEntry ->
                            case newEntry.direction of
                                Across ->
                                    AcrossAndDownStarts newEntry downEntry

                                Down ->
                                    AcrossAndDownStarts acrossEntry newEntry
                )
    in
        Dict.update coordinate updateMaybeEntryStart entryStartDict


keepOnlyJusts : List ( Coordinate, Maybe Entry ) -> List ( Coordinate, Entry )
keepOnlyJusts entryMaybeCoordinateList =
    let
        processMaybeEntry ( c, maybeEntry ) =
            case maybeEntry of
                Just entry ->
                    [ ( c, entry ) ]

                Nothing ->
                    []
    in
        List.map processMaybeEntry entryMaybeCoordinateList
            |> List.concat


acrossList : EntryStartDict -> List ( Coordinate, Entry )
acrossList entryListings =
    entryListings
        |> Dict.toList
        |> List.map (\( k, v ) -> ( k, acrossFromEntryStart v ))
        |> keepOnlyJusts
        |> sortEntries


downList : EntryStartDict -> List ( Coordinate, Entry )
downList entryListings =
    entryListings
        |> Dict.toList
        |> List.map (\( k, v ) -> ( k, downFromEntryStart v ))
        |> keepOnlyJusts
        |> sortEntries


sortEntries : List ( Coordinate, Entry ) -> List ( Coordinate, Entry )
sortEntries =
    List.sortBy (\( c, e ) -> e.number)


acrossFromEntryStart : EntryStart -> Maybe Entry
acrossFromEntryStart entryStart =
    case entryStart of
        AcrossOnlyStart acrossEntry ->
            Just <| entry acrossEntry.number acrossEntry.direction acrossEntry.text acrossEntry.clue

        DownOnlyStart downEntry ->
            Nothing

        AcrossAndDownStarts acrossEntry _ ->
            Just <| entry acrossEntry.number acrossEntry.direction acrossEntry.text acrossEntry.clue


downFromEntryStart : EntryStart -> Maybe Entry
downFromEntryStart entryStart =
    case entryStart of
        AcrossOnlyStart _ ->
            Nothing

        DownOnlyStart downEntry ->
            Just <| entry downEntry.number downEntry.direction downEntry.text downEntry.clue

        AcrossAndDownStarts _ downEntry ->
            Just <| entry downEntry.number downEntry.direction downEntry.text downEntry.clue


entryStartDictFromGrid : Grid -> EntryStartDict
entryStartDictFromGrid grid =
    grid
        |> Matrix.toIndexedArray
        |> Array.foldl (updateFromCoordinate grid) ( 1, Dict.empty )
        |> Tuple.second


updateFromCoordinate : Grid -> ( ( Int, Int ), Square ) -> ( Int, EntryStartDict ) -> ( Int, EntryStartDict )
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
                                    AcrossAndDownStarts
                                        (entry currentEntryNumber Across (acrossEntry grid coord) "")
                                        (entry currentEntryNumber Down (downEntry grid coord) "")
                                else
                                    AcrossOnlyStart
                                        (entry currentEntryNumber Across (acrossEntry grid coord) "")
                            else
                                DownOnlyStart
                                    (entry currentEntryNumber Down (downEntry grid coord) "")
                    in
                        ( nextEntryNumber, Dict.insert coord newEntryStart entriesSoFar )
                else
                    ( currentEntryNumber, entriesSoFar )

        BlockSquare ->
            ( currentEntryNumber, entriesSoFar )


entryMembershipDictFromEntryStartDict : Grid -> EntryStartDict -> EntryMembershipDict
entryMembershipDictFromEntryStartDict grid entryListings =
    grid
        |> Matrix.toIndexedArray
        |> Array.foldl (setEntryMembership grid entryListings) Dict.empty


setEntryMembership : Grid -> EntryStartDict -> ( Coordinate, Square ) -> EntryMembershipDict -> EntryMembershipDict
setEntryMembership grid entryListings ( coord, square ) entryMembershipDict =
    let
        membership =
            case square of
                LetterSquare _ ->
                    BelongsToEntries
                        { across = findAcrossEntryMembership grid entryListings coord
                        , down = findDownEntryMembership grid entryListings coord
                        }

                BlockSquare ->
                    BelongsToNoEntries
    in
        Dict.insert coord membership entryMembershipDict


findAcrossEntryMembership : Grid -> EntryStartDict -> Coordinate -> Int
findAcrossEntryMembership grid entryListings coordinate =
    case Grid.isAcrossEntryStart grid coordinate of
        True ->
            entryNumberAt entryListings coordinate
                |> Maybe.withDefault 0

        False ->
            findAcrossEntryMembership grid entryListings (Coordinate.atLeft coordinate)


findDownEntryMembership : Grid -> EntryStartDict -> Coordinate -> Int
findDownEntryMembership grid entryListings coordinate =
    case Grid.isDownEntryStart grid coordinate of
        True ->
            entryNumberAt entryListings coordinate
                |> Maybe.withDefault 0

        False ->
            findDownEntryMembership grid entryListings (Coordinate.above coordinate)


flattenEntryMembershipDict : EntryMembershipDict -> List ( Coordinate, List Int )
flattenEntryMembershipDict entryMembershipDict =
    Dict.map (\m -> membershipToIntList) entryMembershipDict
        |> Dict.toList


membershipToIntList : EntryMembership -> List Int
membershipToIntList entryMembership =
    case entryMembership of
        BelongsToNoEntries ->
            []

        BelongsToEntries { across, down } ->
            [ across, down ]


acrossEntryMembership : Coordinate -> EntryMembershipDict -> Maybe Int
acrossEntryMembership coordinate entryMembershipDict =
    let
        entryMembership =
            Dict.get coordinate entryMembershipDict
    in
        case entryMembership of
            Just BelongsToNoEntries ->
                Nothing

            Just (BelongsToEntries { across, down }) ->
                Just across

            Nothing ->
                Nothing


downEntryMembership : Coordinate -> EntryMembershipDict -> Maybe Int
downEntryMembership coordinate entryMembershipDict =
    let
        entryMembership =
            Dict.get coordinate entryMembershipDict
    in
        case entryMembership of
            Just BelongsToNoEntries ->
                Nothing

            Just (BelongsToEntries { across, down }) ->
                Just down

            Nothing ->
                Nothing


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
