module Main exposing (..)

import Html exposing (Html, div, text)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Puzzle =
    {}


type alias Model =
    { puzzle : Puzzle
    }


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { puzzle = {} }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( { puzzle = {} }, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "Hello world!" ]
