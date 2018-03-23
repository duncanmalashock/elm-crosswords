module Main exposing (main)

import Views
import Puzzle exposing (Puzzle, EditMode(..))
import Coordinate exposing (Coordinate)
import Grid exposing (Grid)
import Entry exposing (Entry)
import Dict
import Keyboard.Extra exposing (Key(..))
import KeyboardUtils
import Html exposing (Html, div, text, input, label)
import Html.Attributes exposing (type_, id, name, for, value, checked, style)
import Html.Events exposing (onClick)


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
    | ClueEdited Coordinate Entry String
    | ClueEditFocused
    | EditModeClicked EditMode
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
        ( { puzzle = Puzzle.fromString 15 15 stringInput Solving
          , pressedKeys = []
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSquare coordinate ->
            let
                updateSelectionDirectionFn =
                    case model.puzzle.currentSelection of
                        Just ( currentCoordinate, _ ) ->
                            if currentCoordinate == coordinate then
                                (\m -> { m | puzzle = Puzzle.switchSelectionDirection m.puzzle })
                            else
                                identity

                        Nothing ->
                            identity
            in
                ( { model
                    | puzzle =
                        Puzzle.setSelection coordinate model.puzzle
                  }
                    |> updateSelectionDirectionFn
                , Cmd.none
                )

        ClueEdited coordinate entry newClue ->
            ( { model
                | puzzle =
                    Puzzle.updateEntry model.puzzle
                        coordinate
                        { entry | clue = newClue }
              }
            , Cmd.none
            )

        ClueEditFocused ->
            ( { model | puzzle = Puzzle.clearSelection model.puzzle }, Cmd.none )

        EditModeClicked editMode ->
            ( { model | puzzle = Puzzle.setEditMode editMode model.puzzle }, Cmd.none )

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

        updatedPuzzle =
            if (List.member Keyboard.Extra.ArrowLeft changedKeys) then
                Puzzle.moveSelectionLeft model.puzzle
            else if (List.member Keyboard.Extra.ArrowRight changedKeys) then
                Puzzle.moveSelectionRight model.puzzle
            else if (List.member Keyboard.Extra.ArrowUp changedKeys) then
                Puzzle.moveSelectionUp model.puzzle
            else if (List.member Keyboard.Extra.ArrowDown changedKeys) then
                Puzzle.moveSelectionDown model.puzzle
            else if (List.member Keyboard.Extra.Space changedKeys) then
                Puzzle.switchSelectionDirection model.puzzle
            else if (KeyboardUtils.containsLetterKeys changedKeys) then
                Puzzle.typeLetters changedKeys model.puzzle
            else if (List.member Keyboard.Extra.BackSpace changedKeys) then
                Puzzle.deleteLetter model.puzzle
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
            let
                clues =
                    case model.puzzle.editMode of
                        Solving ->
                            Views.cluesView
                                model.puzzle.currentSelection
                                model.puzzle.entryMembershipDict
                                model.puzzle.entryStartDict

                        Editing ->
                            Views.cluesEditView
                                model.puzzle.currentSelection
                                model.puzzle.entryMembershipDict
                                model.puzzle.entryStartDict
                                ClueEdited
                                ClueEditFocused
            in
                div []
                    [ Views.gridView grid
                        model.puzzle.currentSelection
                        model.puzzle.entryStartDict
                        model.puzzle.entryMembershipDict
                        ClickedSquare
                    , clues
                    , toggleEditorView model
                    ]

        Err string ->
            div [] [ text "couldn't load!" ]


toggleEditorView : Model -> Html Msg
toggleEditorView model =
    div
        [ style
            [ ( "margin-top", "20px" )
            ]
        ]
        [ input
            [ type_ "radio"
            , id "solving"
            , name "mode"
            , value "solving"
            , checked (model.puzzle.editMode == Solving)
            , onClick (EditModeClicked Solving)
            ]
            []
        , label
            [ for "solving" ]
            [ text "Solving" ]
        , input
            [ type_ "radio"
            , id "editing"
            , name "mode"
            , value "editing"
            , checked (model.puzzle.editMode == Editing)
            , onClick (EditModeClicked Editing)
            , style
                [ ( "margin-left", "10px" )
                ]
            ]
            []
        , label
            [ for "editing" ]
            [ text "Editing" ]
        ]
