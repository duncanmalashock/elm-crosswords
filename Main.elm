module Main exposing (main)

import Views
import DesignSystem.Layout.Divider as Divider
import Puzzle exposing (Puzzle, EditMode(..))
import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Grid exposing (Grid)
import Dict
import Keyboard.Extra exposing (Key(..))
import KeyboardUtils
import Date
import Html
import Html.Styled exposing (..)
import Html.Attributes exposing (type_, id, name, for, value, checked, style)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { puzzle : Result String Puzzle
    , pressedKeys : List Key
    }


type Msg
    = ClickedSquare Coordinate
    | KeyboardMsg Keyboard.Extra.Msg
    | ClickedClue Int Direction


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map KeyboardMsg Keyboard.Extra.subscriptions


init : ( Model, Cmd Msg )
init =
    let
        stringInput =
            [ "*STEP"
            , "CHINA"
            , "HINDI"
            , "ANGEL"
            , "DYED*"
            ]
                |> String.concat

        clues =
            { across =
                Dict.fromList
                    [ ( 1, "Aerobics class with an elevated block" )
                    , ( 5, "U.S. tariff war opponent" )
                    , ( 6, "Official language of India" )
                    , ( 7, "Heavenly figure in Michelangelo's \"The Last Judgement\"" )
                    , ( 8, "Changed the color of" )
                    ]
            , down =
                Dict.fromList
                    [ ( 1, "Like a very distracting object" )
                    , ( 2, "Hint of color" )
                    , ( 3, "Brought to a close" )
                    , ( 4, "Sand castle mold" )
                    , ( 5, "Country between Niger and Sudan" )
                    ]
            }
    in
        ( { puzzle = Puzzle.fromString 5 5 stringInput "Doink" clues (Date.fromTime (12343234)) Solving
          , pressedKeys = []
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSquare coordinate ->
            let
                updateSelectionDirectionFn puzzle =
                    case puzzle.currentSelection of
                        Just ( currentCoordinate, _ ) ->
                            if currentCoordinate == coordinate then
                                Puzzle.switchSelectionDirection puzzle
                            else
                                puzzle

                        Nothing ->
                            puzzle
            in
                ( { model
                    | puzzle =
                        Result.map updateSelectionDirectionFn model.puzzle
                            |> Result.map (Puzzle.setSelection coordinate)
                  }
                , Cmd.none
                )

        KeyboardMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.Extra.update keyMsg model.pressedKeys
            in
                updateWithNewPressedKeys newPressedKeys model

        ClickedClue entryNumber direction ->
            ( { model
                | puzzle =
                    Result.map (Puzzle.selectEntry direction entryNumber) model.puzzle
              }
            , Cmd.none
            )


updateWithNewPressedKeys : List Key -> Model -> ( Model, Cmd Msg )
updateWithNewPressedKeys newPressedKeys model =
    let
        changedKeys =
            List.filter
                (\k -> not (List.member k model.pressedKeys))
                newPressedKeys

        updatedPuzzle puzzle =
            if (List.member Keyboard.Extra.ArrowLeft changedKeys) then
                Puzzle.moveSelectionLeft puzzle
            else if (List.member Keyboard.Extra.ArrowRight changedKeys) then
                Puzzle.moveSelectionRight puzzle
            else if (List.member Keyboard.Extra.ArrowUp changedKeys) then
                Puzzle.moveSelectionUp puzzle
            else if (List.member Keyboard.Extra.ArrowDown changedKeys) then
                Puzzle.moveSelectionDown puzzle
            else if (List.member Keyboard.Extra.Space changedKeys) then
                Puzzle.switchSelectionDirection puzzle
            else if (KeyboardUtils.containsLetterKeys changedKeys) then
                (Puzzle.typeLetters changedKeys) puzzle
            else if (List.member Keyboard.Extra.BackSpace changedKeys) then
                Puzzle.deleteLetter puzzle
            else
                puzzle
    in
        ( { model
            | pressedKeys = newPressedKeys
            , puzzle =
                Result.map updatedPuzzle model.puzzle
                    |> Result.map Puzzle.updateCompletionState
          }
        , Cmd.none
        )


view : Model -> Html Msg
view model =
    case model.puzzle of
        Ok puzzle ->
            let
                gridView =
                    [ Views.completedView puzzle
                    , Views.gridView puzzle.grid
                        puzzle.currentSelection
                        ClickedSquare
                    ]

                cluesView =
                    List.map (Views.clueView puzzle.grid Across puzzle.currentSelection ClickedClue) (Grid.acrossClues puzzle.grid)
                        ++ List.map (Views.clueView puzzle.grid Down puzzle.currentSelection ClickedClue) (Grid.downClues puzzle.grid)
            in
                Divider.divider
                    [ ( {}, List.map fromUnstyled gridView )
                    , ( {}, List.map fromUnstyled cluesView )
                    ]

        Err error ->
            text <| "Couldn't load puzzle: " ++ error
