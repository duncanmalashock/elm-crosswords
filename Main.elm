module Main exposing (main)

import Views
import Puzzle exposing (Puzzle)
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
    , message : String
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
          , message = ""
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSquare coordinate ->
            ( { model | puzzle = Puzzle.setSelection coordinate model.puzzle }
            , Cmd.none
            )

        KeyboardMsg keyMsg ->
            let
                newPressedKeys =
                    Keyboard.Extra.update keyMsg model.pressedKeys

                changedKeys =
                    List.filter (\k -> not (List.member k model.pressedKeys)) newPressedKeys

                newMessage =
                    if (List.member Keyboard.Extra.CharA changedKeys) then
                        "A"
                    else if (List.member Keyboard.Extra.CharB changedKeys) then
                        "B"
                    else if (List.member Keyboard.Extra.CharC changedKeys) then
                        "C"
                    else
                        ""
            in
                ( { model
                    | pressedKeys = newPressedKeys
                    , message = newMessage
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div []
        [ Views.gridView
            (model.puzzle.grid |> Result.withDefault Grid.empty)
            model.puzzle.currentSelection
            (Entry.allFromGrid (model.puzzle.grid |> Result.withDefault Grid.empty))
            ClickedSquare
        ]
