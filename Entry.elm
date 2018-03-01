module Entry exposing (Entry(..), Direction(..), allFromGrid)

import Grid exposing (Grid, Square(..))


type Entry
    = Entry Int Direction


type Direction
    = Across
    | Down


allFromGrid : Grid -> List Entry
allFromGrid grid =
    allFromGridHelp 1 grid grid


allFromGridHelp : Int -> List Square -> Grid -> List Entry
allFromGridHelp currentEntryNumber squares grid =
    case squares of
        [] ->
            []

        first :: rest ->
            case first of
                LetterSquare ( x, y ) _ ->
                    let
                        acrossEntry =
                            if Grid.isAcrossEntryStart grid first then
                                [ Entry currentEntryNumber Across ]
                            else
                                []

                        downEntry =
                            if Grid.isDownEntryStart grid first then
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
                        entries
                            ++ allFromGridHelp nextEntryNumber rest grid

                BlockSquare ( x, y ) ->
                    allFromGridHelp currentEntryNumber rest grid
