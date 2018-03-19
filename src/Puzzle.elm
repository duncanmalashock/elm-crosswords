module Puzzle
    exposing
        ( Puzzle
        , Selection
        , SelectionPermit(..)
        , fromString
        , selection
        , selectionCoordinate
        , switchSelectionDirection
        , setSelection
        , clearSelection
        , moveSelectionLeft
        , moveSelectionRight
        , moveSelectionUp
        , moveSelectionDown
        , typeLetters
        , deleteLetter
        , updateEntry
        )

import Grid exposing (Grid)
import Entry exposing (Entry)
import Direction exposing (Direction(..))
import Coordinate exposing (Coordinate)
import KeyboardUtils
import Keyboard.Extra exposing (Key(..))
import Dict


type alias Puzzle =
    { grid : Result String Grid
    , currentSelection : Maybe Selection
    , entryStartDict : Entry.EntryStartDict
    , entryMembershipDict : Entry.EntryMembershipDict
    }


type alias Selection =
    ( Coordinate, Direction )


type SelectionPermit
    = CanSelectAllSquares
    | CanSelectOnlyLetterSquares


fromString : Int -> Int -> String -> Puzzle
fromString gridWidth gridHeight string =
    let
        gridResult =
            Grid.fromString gridWidth gridHeight string

        gridDefault =
            Result.withDefault (Grid.blank 1 1) gridResult

        entryStartDict =
            Entry.entryStartDictFromGrid gridDefault

        entryMembershipDict =
            Entry.entryMembershipDictFromEntryStartDict gridDefault entryStartDict
    in
        { grid = Result.map Grid.clear gridResult
        , currentSelection = Nothing
        , entryStartDict = entryStartDict
        , entryMembershipDict = entryMembershipDict
        }


typeLetters : List Key -> SelectionPermit -> Puzzle -> Puzzle
typeLetters keyList selectionPermit puzzle =
    List.foldl (\k -> typeLetter (KeyboardUtils.toLetterChar k) selectionPermit)
        puzzle
        (KeyboardUtils.filterOnlyLetterKeys keyList)


typeLetter : Char -> SelectionPermit -> Puzzle -> Puzzle
typeLetter char permit puzzle =
    case puzzle.currentSelection of
        Just ( coord, direction ) ->
            case direction of
                Across ->
                    { puzzle | grid = Result.map (Grid.updateLetterSquare coord char) puzzle.grid }
                        |> moveSelectionRight permit

                Down ->
                    { puzzle | grid = Result.map (Grid.updateLetterSquare coord char) puzzle.grid }
                        |> moveSelectionDown permit

        Nothing ->
            puzzle


deleteLetter : SelectionPermit -> Puzzle -> Puzzle
deleteLetter permit puzzle =
    case puzzle.currentSelection of
        Just ( coord, direction ) ->
            case direction of
                Across ->
                    { puzzle | grid = Result.map (Grid.updateLetterSquare coord ' ') puzzle.grid }
                        |> moveSelectionLeft permit

                Down ->
                    { puzzle | grid = Result.map (Grid.updateLetterSquare coord ' ') puzzle.grid }
                        |> moveSelectionUp permit

        Nothing ->
            puzzle


selection : Puzzle -> Maybe Selection
selection puzzle =
    puzzle.currentSelection


selectionMapToCoordinate : (Coordinate -> Coordinate) -> Selection -> Selection
selectionMapToCoordinate function ( coord, direction ) =
    ( function coord, direction )


selectionCoordinate : Selection -> Coordinate
selectionCoordinate ( coord, direction ) =
    coord


switchSelectionDirection : Puzzle -> Puzzle
switchSelectionDirection puzzle =
    let
        updatedSelection =
            case puzzle.currentSelection of
                Just ( coord, direction ) ->
                    case direction of
                        Across ->
                            Just ( coord, Down )

                        Down ->
                            Just ( coord, Across )

                Nothing ->
                    Nothing
    in
        { puzzle | currentSelection = updatedSelection }


moveSelectionLeft : SelectionPermit -> Puzzle -> Puzzle
moveSelectionLeft permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.atLeft permit puzzle


moveSelectionRight : SelectionPermit -> Puzzle -> Puzzle
moveSelectionRight permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.atRight permit puzzle


moveSelectionUp : SelectionPermit -> Puzzle -> Puzzle
moveSelectionUp permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.above permit puzzle


moveSelectionDown : SelectionPermit -> Puzzle -> Puzzle
moveSelectionDown permit puzzle =
    moveSelection puzzle.currentSelection Coordinate.below permit puzzle


moveSelection : Maybe Selection -> (Coordinate -> Coordinate) -> SelectionPermit -> Puzzle -> Puzzle
moveSelection startingSelection newCoordFn permit puzzle =
    case puzzle.currentSelection of
        Just selection ->
            let
                ( newCoordToTry, direction ) =
                    selectionMapToCoordinate newCoordFn selection

                newCoordIsInBounds grid =
                    Grid.coordIsInBounds grid newCoordToTry

                newSquareIsPermitted grid =
                    (Grid.hasLetterSquareAt grid newCoordToTry)
                        || (permit == CanSelectAllSquares)
            in
                case (Result.map newCoordIsInBounds puzzle.grid) of
                    Ok True ->
                        case (Result.map newSquareIsPermitted puzzle.grid) of
                            Ok True ->
                                setSelection newCoordToTry permit puzzle

                            Ok False ->
                                moveSelection
                                    startingSelection
                                    newCoordFn
                                    permit
                                    { puzzle
                                        | currentSelection =
                                            Maybe.map (\s -> selectionMapToCoordinate newCoordFn s)
                                                puzzle.currentSelection
                                    }

                            Err _ ->
                                puzzle

                    Ok False ->
                        { puzzle | currentSelection = startingSelection }

                    Err _ ->
                        puzzle

        Nothing ->
            puzzle


setSelection : Coordinate -> SelectionPermit -> Puzzle -> Puzzle
setSelection (( x, y ) as coordinate) permit puzzle =
    case puzzle.grid of
        Ok grid ->
            let
                isInXBounds xVal =
                    xVal >= 0 && xVal < (Grid.width grid)

                isInYBounds yVal =
                    yVal >= 0 && yVal < (Grid.height grid)

                squareIsPermitted =
                    (Grid.hasLetterSquareAt grid coordinate)
                        || (permit == CanSelectAllSquares)
            in
                if (isInXBounds x && isInYBounds y && squareIsPermitted) then
                    case puzzle.currentSelection of
                        Just selection ->
                            { puzzle
                                | currentSelection =
                                    Maybe.map
                                        (selectionMapToCoordinate (\_ -> coordinate))
                                        puzzle.currentSelection
                            }

                        Nothing ->
                            { puzzle | currentSelection = Just ( coordinate, Across ) }
                else
                    puzzle

        Err _ ->
            { puzzle | currentSelection = Nothing }


clearSelection : Puzzle -> Puzzle
clearSelection puzzle =
    { puzzle | currentSelection = Nothing }


updateEntry : Puzzle -> Coordinate -> Entry -> Puzzle
updateEntry puzzle coordinate newClue =
    { puzzle | entryStartDict = Entry.updateEntry puzzle.entryStartDict coordinate newClue }
