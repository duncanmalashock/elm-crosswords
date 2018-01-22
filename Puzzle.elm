module Puzzle exposing (entriesFromGrid)

import Grid exposing (Grid)
import Entry exposing (Entry, Direction(..))


entriesFromGrid : Grid -> List Entry
entriesFromGrid grid =
    (acrossEntriesFromGrid grid) ++ (downEntriesFromGrid grid)


acrossEntriesFromGrid : Grid -> List Entry
acrossEntriesFromGrid grid =
    List.filter (Grid.isAcrossEntryStart grid) grid
        |> List.map
            (\square ->
                { startingCoordinates = Grid.squareCoordinates square
                , direction = Across
                , length = -1
                }
            )


downEntriesFromGrid : Grid -> List Entry
downEntriesFromGrid grid =
    List.filter (Grid.isDownEntryStart grid) grid
        |> List.map
            (\square ->
                { startingCoordinates = Grid.squareCoordinates square
                , direction = Down
                , length = -1
                }
            )
