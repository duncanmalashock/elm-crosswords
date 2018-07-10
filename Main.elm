module Main exposing (main)

import Views
import Puzzle exposing (Puzzle, EditMode(..))
import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Grid exposing (Grid)
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
    | KeyboardMsg Keyboard.Extra.Msg


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
        ( { puzzle = Puzzle.fromString 5 5 stringInput clues Solving
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
            , puzzle =
                updatedPuzzle
                    |> Puzzle.updateCompletionState
          }
        , Cmd.none
        )


clueView : ( Int, String ) -> Html Msg
clueView ( clueNumber, clueString ) =
    div []
        [ text <| (toString clueNumber) ++ ": " ++ clueString ]


view : Model -> Html Msg
view model =
    case model.puzzle.grid of
        Ok grid ->
            div [] <|
                [ Views.completedView model.puzzle
                , Views.gridView grid
                    model.puzzle.currentSelection
                    ClickedSquare
                ]
                    ++ List.map clueView (Grid.acrossClues grid)
                    ++ List.map clueView (Grid.downClues grid)

        Err string ->
            div [] [ text "couldn't load!" ]
