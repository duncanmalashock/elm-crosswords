module Entry exposing (Entry, EntryStart(..), allFromGrid)

import Coordinate exposing (Coordinate)
import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


type alias Entry =
    String


type EntryStart
    = AcrossOnly Int Entry
    | DownOnly Int Entry
    | AcrossAndDown Int Entry Entry


allFromGrid : Grid -> List EntryStart
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
