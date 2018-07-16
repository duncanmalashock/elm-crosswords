module Puzzle
    exposing
        ( Puzzle
        , Selection
        , EditMode(..)
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
        , setEditMode
        , selectEntry
        , setGuess
        , updateCompletionState
        )

import Grid exposing (Grid, CompletionState(..), CluesDict)
import Direction exposing (Direction(..))
import Coordinate exposing (Coordinate)
import Date exposing (Date)
import KeyboardUtils
import Keyboard.Extra exposing (Key(..))
import Maybe.Extra as Maybe
import Dict


type alias Puzzle =
    { grid : Grid
    , author : AuthorInfo
    , createdAt : Date
    , currentSelection : Maybe Selection
    , editMode : EditMode
    , completed : CompletionState
    }


type alias Selection =
    ( Coordinate, Direction )


type EditMode
    = Solving
    | Editing


type alias AuthorInfo =
    { name : String
    }


fromString : Int -> Int -> String -> String -> CluesDict -> Date -> EditMode -> Result String Puzzle
fromString gridWidth gridHeight gridString authorName clues createdAt editMode =
    Grid.fromString gridWidth gridHeight gridString clues
        |> Result.map
            (\grid ->
                { grid = grid
                , author = { name = authorName }
                , createdAt = createdAt
                , currentSelection = Nothing
                , editMode = editMode
                , completed = NotCompleted
                }
            )


typeLetters : List Key -> Puzzle -> Puzzle
typeLetters keyList puzzle =
    List.foldl (\k -> typeLetter (KeyboardUtils.toLetterChar k))
        puzzle
        (KeyboardUtils.filterOnlyLetterKeys keyList)


typeLetter : Char -> Puzzle -> Puzzle
typeLetter char puzzle =
    case puzzle.currentSelection of
        Just ( coord, direction ) ->
            case direction of
                Across ->
                    { puzzle | grid = Grid.setGuess coord char puzzle.grid }
                        |> moveSelectionRight

                Down ->
                    { puzzle | grid = Grid.setGuess coord char puzzle.grid }
                        |> moveSelectionDown

        Nothing ->
            puzzle


deleteLetter : Puzzle -> Puzzle
deleteLetter puzzle =
    case puzzle.currentSelection of
        Just ( coord, direction ) ->
            case direction of
                Across ->
                    { puzzle | grid = Grid.setGuess coord ' ' puzzle.grid }
                        |> moveSelectionLeft

                Down ->
                    { puzzle | grid = Grid.setGuess coord ' ' puzzle.grid }
                        |> moveSelectionUp

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


moveSelectionLeft : Puzzle -> Puzzle
moveSelectionLeft puzzle =
    moveSelection puzzle.currentSelection Coordinate.atLeft puzzle


moveSelectionRight : Puzzle -> Puzzle
moveSelectionRight puzzle =
    moveSelection puzzle.currentSelection Coordinate.atRight puzzle


moveSelectionUp : Puzzle -> Puzzle
moveSelectionUp puzzle =
    moveSelection puzzle.currentSelection Coordinate.above puzzle


moveSelectionDown : Puzzle -> Puzzle
moveSelectionDown puzzle =
    moveSelection puzzle.currentSelection Coordinate.below puzzle


moveSelection : Maybe Selection -> (Coordinate -> Coordinate) -> Puzzle -> Puzzle
moveSelection startingSelection newCoordFn puzzle =
    case puzzle.currentSelection of
        Just selection ->
            let
                ( newCoordToTry, direction ) =
                    selectionMapToCoordinate newCoordFn selection

                newCoordIsInBounds grid =
                    Grid.coordIsInBounds grid newCoordToTry

                newSquareIsPermitted grid =
                    (Grid.hasLetterSquareAt grid newCoordToTry)
                        || (puzzle.editMode == Editing)
            in
                case newCoordIsInBounds puzzle.grid of
                    True ->
                        case newSquareIsPermitted puzzle.grid of
                            True ->
                                setSelection newCoordToTry puzzle

                            False ->
                                moveSelection
                                    startingSelection
                                    newCoordFn
                                    { puzzle
                                        | currentSelection =
                                            Maybe.map (\s -> selectionMapToCoordinate newCoordFn s)
                                                puzzle.currentSelection
                                    }

                    False ->
                        { puzzle | currentSelection = startingSelection }

        Nothing ->
            puzzle


setSelection : Coordinate -> Puzzle -> Puzzle
setSelection (( x, y ) as coordinate) puzzle =
    let
        isInXBounds xVal =
            xVal >= 0 && xVal < (Grid.width puzzle.grid)

        isInYBounds yVal =
            yVal >= 0 && yVal < (Grid.height puzzle.grid)

        squareIsPermitted =
            (Grid.hasLetterSquareAt puzzle.grid coordinate)
                || (puzzle.editMode == Editing)
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


clearSelection : Puzzle -> Puzzle
clearSelection puzzle =
    { puzzle | currentSelection = Nothing }


selectEntry : Direction -> Int -> Puzzle -> Puzzle
selectEntry direction entryNumber puzzle =
    let
        selection =
            Grid.entryCoordinate direction entryNumber puzzle.grid
                |> Maybe.map (\num -> ( num, direction ))
    in
        { puzzle | currentSelection = selection }


setEditMode : EditMode -> Puzzle -> Puzzle
setEditMode editMode puzzle =
    { puzzle | editMode = editMode }


setGuess : Puzzle -> Coordinate -> Char -> Puzzle
setGuess puzzle coordinate char =
    { puzzle | grid = Grid.setGuess coordinate char puzzle.grid }


updateCompletionState : Puzzle -> Puzzle
updateCompletionState puzzle =
    { puzzle | completed = Grid.completionState puzzle.grid }
