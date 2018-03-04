module Entry exposing (Entry, EntryStart(..), allFromGrid)

import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


type alias Entry =
    {}


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
atIndex grid ( ( x, y ), square ) ( currentEntryNumber, entriesSoFar ) =
    case square of
        LetterSquare _ ->
            let
                newEntryStart =
                    if Grid.isAcrossEntryStart grid ( x, y ) then
                        if Grid.isDownEntryStart grid ( x, y ) then
                            [ AcrossAndDown currentEntryNumber {} {} ]
                        else
                            [ AcrossOnly currentEntryNumber {} ]
                    else if Grid.isDownEntryStart grid ( x, y ) then
                        [ DownOnly currentEntryNumber {} ]
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
