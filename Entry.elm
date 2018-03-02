module Entry exposing (Entry(..), Direction(..), allFromGrid)

import Grid exposing (Grid, Square(..))


type Entry
    = Entry Int Direction


type Direction
    = Across
    | Down


allFromGrid : Grid -> List Entry
allFromGrid grid =
    allFromGridHelp 1 grid grid []


allFromGridHelp : Int -> List Square -> Grid -> List Entry -> List Entry
allFromGridHelp currentEntryNumber squares grid entriesSoFar =
    case squares of
        [] ->
            List.reverse entriesSoFar

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
                            downEntry ++ acrossEntry

                        nextEntryNumber =
                            if List.isEmpty entries then
                                currentEntryNumber
                            else
                                currentEntryNumber + 1
                    in
                        allFromGridHelp nextEntryNumber rest grid (entries ++ entriesSoFar)

                BlockSquare ( x, y ) ->
                    allFromGridHelp currentEntryNumber rest grid entriesSoFar
