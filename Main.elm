module Main exposing (main)

import Views
import Puzzle exposing (Puzzle, Direction(..), SelectionPermit(..))
import Coordinate exposing (Coordinate)
import Grid exposing (Grid)
import Entry exposing (EntryListings)
import Dict
import Keyboard.Extra exposing (Key(..))
import Html exposing (Html, div, text)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { puzzle : Puzzle
    , pressedKeys : List Key
    }


type Msg
    = ClickedSquare Coordinate
    | KeyboardMsg Keyboard.Extra.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardMsg Keyboard.Extra.subscriptions


init : ( Model, Cmd Msg )
init =
    let
        stringInput =
            [ "HAZY*BRECHT*ECO"
            , "OREO*MULLAH*COX"
            , "STAYSINSIDE*URI"
            , "**LOW*TACOSTAND"
            , "THOMAS***NOIDEA"
            , "HITANERVE*UBOAT"
            , "YDS**REIN*TERSE"
            , "***LEFTRIGHT***"
            , "STREP*ONAT**ATL"
            , "THANI*WACOTEXAS"
            , "ARMADA***SAILED"
            , "MORSECODE*LTR**"
            , "IWO*MUSICSCHOOL"
            , "NOD*ITHACA*ESSO"
            , "ANS*CEASED*REST"
            ]
                |> String.concat
    in
        ( { puzzle = Puzzle.fromString 15 15 stringInput
          , pressedKeys = []
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSquare coordinate ->
            ( { model | puzzle = Puzzle.setSelection ( coordinate, Across ) CanSelectOnlyLetterSquares model.puzzle }
            , Cmd.none
            )

        KeyboardMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.Extra.update keyMsg model.pressedKeys
            in
                updateWithNewPressedKeys newPressedKeys model


updateWithNewPressedKeys : List Key -> Model -> ( Model, Cmd Msg )
updateWithNewPressedKeys newPressedKeys model =
    let
        changedKeys =
            List.filter
                (\k -> not (List.member k model.pressedKeys))
                newPressedKeys

        letterKeys =
            [ Keyboard.Extra.CharA
            , Keyboard.Extra.CharB
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

        filterLetterKeys keyList =
            List.filter (\k -> List.member k letterKeys) keyList

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

        updatedPuzzle =
            if (List.member Keyboard.Extra.ArrowLeft changedKeys) then
                Puzzle.moveSelectionLeft CanSelectOnlyLetterSquares model.puzzle
            else if (List.member Keyboard.Extra.ArrowRight changedKeys) then
                Puzzle.moveSelectionRight CanSelectOnlyLetterSquares model.puzzle
            else if (List.member Keyboard.Extra.ArrowUp changedKeys) then
                Puzzle.moveSelectionUp CanSelectOnlyLetterSquares model.puzzle
            else if (List.member Keyboard.Extra.ArrowDown changedKeys) then
                Puzzle.moveSelectionDown CanSelectOnlyLetterSquares model.puzzle
            else if (List.member Keyboard.Extra.Space changedKeys) then
                Puzzle.switchSelectionDirection model.puzzle
            else if (containsLetterKeys changedKeys) then
                List.foldl (\k -> Puzzle.typeLetter (toLetterChar k)) model.puzzle (filterLetterKeys changedKeys)
            else
                model.puzzle
    in
        ( { model
            | pressedKeys = newPressedKeys
            , puzzle = updatedPuzzle
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    case model.puzzle.grid of
        Ok grid ->
            div []
                [ Views.gridView grid
                    model.puzzle.currentSelection
                    model.puzzle.entryStarts
                    model.puzzle.entryMemberships
                    ClickedSquare
                ]

        Err string ->
            div [] [ text "couldn't load!" ]
