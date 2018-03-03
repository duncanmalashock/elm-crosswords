module Entry exposing (Entry(..), Direction(..), allFromGrid)

import Grid exposing (Grid, Square(..))
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


type Entry
    = Entry Int Direction


type Direction
    = Across
    | Down


allFromGrid : Grid -> List Entry
allFromGrid grid =
    grid
        |> Matrix.toIndexedArray
        |> Array.foldl (atIndex grid) ( 1, [] )
        |> Tuple.second


atIndex : Grid -> ( ( Int, Int ), Square ) -> ( Int, List Entry ) -> ( Int, List Entry )
atIndex grid ( ( x, y ), square ) ( currentEntryNumber, entriesSoFar ) =
    case square of
        LetterSquare _ ->
            let
                acrossEntry =
                    if Grid.isAcrossEntryStart grid ( x, y ) then
                        [ Entry currentEntryNumber Across ]
                    else
                        []

                downEntry =
                    if Grid.isDownEntryStart grid ( x, y ) then
                        [ Entry currentEntryNumber Down ]
                    else
                        []

                entries =
                    acrossEntry ++ downEntry

                nextEntryNumber =
                    if List.isEmpty entries then
                        currentEntryNumber
                    else
                        currentEntryNumber + 1
            in
                ( nextEntryNumber, entriesSoFar ++ entries )

        BlockSquare ->
            ( currentEntryNumber, entriesSoFar )
