module Clue exposing (ClueDict, Clue, clueDictfromEntryStartDict, across, down)

import Coordinate exposing (Coordinate)
import Entry exposing (EntryStartDict, EntryStart(..))
import Dict exposing (Dict)
import Maybe.Extra as Maybe
import Direction exposing (Direction(..))


type alias ClueDict =
    Dict Coordinate IndexClues


type IndexClues
    = AcrossOnlyClue Int String
    | DownOnlyClue Int String
    | AcrossAndDownClues Int String String


type alias Clue =
    { index : Int
    , coordinate : Coordinate
    , direction : Direction
    , clue : String
    }


clue : Int -> Coordinate -> Direction -> String -> Clue
clue index coordinate direction clueString =
    { index = index
    , coordinate = coordinate
    , direction = direction
    , clue = clueString
    }


clueDictfromEntryStartDict : EntryStartDict -> ClueDict
clueDictfromEntryStartDict entryStartDict =
    Dict.foldl (\c es d -> updateClueDictFromEntryStart c es d) Dict.empty entryStartDict


updateClueDictFromEntryStart : Coordinate -> EntryStart -> ClueDict -> ClueDict
updateClueDictFromEntryStart coordinate entryStart clueDict =
    case entryStart of
        AcrossOnlyStart i a ->
            Dict.update coordinate (\v -> Just <| AcrossOnlyClue i "") clueDict

        DownOnlyStart i d ->
            Dict.update coordinate (\v -> Just <| DownOnlyClue i "") clueDict

        AcrossAndDownStarts i a d ->
            Dict.update coordinate (\v -> Just <| AcrossAndDownClues i "" "") clueDict


clueIndex : Clue -> Int
clueIndex clue =
    clue.index


across : ClueDict -> List Clue
across clueDict =
    Dict.keys clueDict
        |> List.map
            (\k ->
                Dict.get k clueDict
                    |> Maybe.map (acrossFromIndexClues k)
            )
        |> Maybe.values
        |> Maybe.values
        |> List.sortBy clueIndex


down : ClueDict -> List Clue
down clueDict =
    Dict.keys clueDict
        |> List.map
            (\k ->
                Dict.get k clueDict
                    |> Maybe.map (downFromIndexClues k)
            )
        |> Maybe.values
        |> Maybe.values
        |> List.sortBy clueIndex


acrossFromIndexClues : Coordinate -> IndexClues -> Maybe Clue
acrossFromIndexClues coordinate indexClues =
    case indexClues of
        AcrossOnlyClue int acrossClueString ->
            Just <| clue int coordinate Across acrossClueString

        DownOnlyClue _ _ ->
            Nothing

        AcrossAndDownClues int acrossClueString _ ->
            Just <| clue int coordinate Across acrossClueString


downFromIndexClues : Coordinate -> IndexClues -> Maybe Clue
downFromIndexClues coordinate indexClues =
    case indexClues of
        AcrossOnlyClue _ _ ->
            Nothing

        DownOnlyClue int downClueString ->
            Just <| clue int coordinate Down downClueString

        AcrossAndDownClues int _ downClueString ->
            Just <| clue int coordinate Down downClueString
