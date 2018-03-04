module Main exposing (main)

import Grid exposing (Grid)
import Entry
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
    { grid : Grid
    }


type Msg
    = NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { grid =
            Grid.fromString 4 4 "ABCDEF*GH*IJ*KLM"
                |> Result.withDefault Grid.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Grid.view model.grid
        , Entry.view model.grid
        ]
