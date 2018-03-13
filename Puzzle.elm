module Puzzle
    exposing
        ( Puzzle
        , Selection
        , Direction(..)
        , SelectionPermit(..)
        , fromString
        , selection
        , selectionCoordinate
        , switchSelectionDirection
        , setSelection
        , moveSelectionLeft
        , moveSelectionRight
        , moveSelectionUp
        , moveSelectionDown
        , containsLetterKeys
        , typeLetters
        , deleteLetter
        )

import Grid exposing (Grid)
import Entry
import Coordinate exposing (Coordinate)
import Keyboard.Extra exposing (Key(..))


type alias Puzzle =
    { grid : Result String Grid
    , currentSelection : Maybe Selection
    , entryStarts : Entry.EntryListings
    , entryMemberships : Entry.EntryMemberships
    }


type alias Selection =
    ( Coordinate, Direction )


type Direction
    = Across
    | Down


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

        entryStarts =
            Entry.allFromGrid gridDefault

        entryMemberships =
            Entry.entryMembershipsFromEntryListings gridDefault entryStarts
    in
        { grid = gridResult
        , currentSelection = Nothing
        , entryStarts = entryStarts
        , entryMemberships = entryMemberships
        }


letterKeys =
    [ Keyboard.Extra.CharA
    , Keyboard.Extra.CharB
    , Keyboard.Extra.CharC
    , Keyboard.Extra.CharD
    , Keyboard.Extra.CharE
    , Keyboard.Extra.CharF
    , Keyboard.Extra.CharG
    , Keyboard.Extra.CharH
    , Keyboard.Extra.CharI
    , Keyboard.Extra.CharJ
    , Keyboard.Extra.CharK
    , Keyboard.Extra.CharL
    , Keyboard.Extra.CharM
    , Keyboard.Extra.CharN
    , Keyboard.Extra.CharO
    , Keyboard.Extra.CharP
    , Keyboard.Extra.CharQ
    , Keyboard.Extra.CharR
    , Keyboard.Extra.CharS
    , Keyboard.Extra.CharT
    , Keyboard.Extra.CharU
    , Keyboard.Extra.CharV
    , Keyboard.Extra.CharW
    , Keyboard.Extra.CharX
    , Keyboard.Extra.CharY
    , Keyboard.Extra.CharZ
    ]


containsLetterKeys keyList =
    List.any (\k -> List.member k letterKeys) keyList


filterOnlyLetterKeys : List Key -> List Key
filterOnlyLetterKeys keyList =
    List.filter (\k -> List.member k letterKeys) keyList


toLetterChar : Key -> Char
toLetterChar k =
    case k of
        Keyboard.Extra.CharA ->
            'A'

        Keyboard.Extra.CharB ->
            'B'

        Keyboard.Extra.CharC ->
            'C'

        Keyboard.Extra.CharD ->
            'D'

        Keyboard.Extra.CharE ->
            'E'

        Keyboard.Extra.CharF ->
            'F'

        Keyboard.Extra.CharG ->
            'G'

        Keyboard.Extra.CharH ->
            'H'

        Keyboard.Extra.CharI ->
            'I'

        Keyboard.Extra.CharJ ->
            'J'

        Keyboard.Extra.CharK ->
            'K'

        Keyboard.Extra.CharL ->
            'L'

        Keyboard.Extra.CharM ->
            'M'

        Keyboard.Extra.CharN ->
            'N'

        Keyboard.Extra.CharO ->
            'O'

        Keyboard.Extra.CharP ->
            'P'

        Keyboard.Extra.CharQ ->
            'Q'

        Keyboard.Extra.CharR ->
            'R'

        Keyboard.Extra.CharS ->
            'S'

        Keyboard.Extra.CharT ->
            'T'

        Keyboard.Extra.CharU ->
            'U'

        Keyboard.Extra.CharV ->
            'V'

        Keyboard.Extra.CharW ->
            'W'

        Keyboard.Extra.CharX ->
            'X'

        Keyboard.Extra.CharY ->
            'Y'

        Keyboard.Extra.CharZ ->
            'Z'

        _ ->
            ' '


typeLetters : List Key -> SelectionPermit -> Puzzle -> Puzzle
typeLetters keyList selectionPermit puzzle =
    List.foldl (\k -> typeLetter (toLetterChar k) selectionPermit)
        puzzle
        (filterOnlyLetterKeys keyList)


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
