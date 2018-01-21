module Main exposing (main)

import Grid exposing (Grid)
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
    ( { grid = Grid.initGrid }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Grid.view model.grid


type Direction
    = Across
    | Down



-- startsEntry : Grid -> Square -> Bool
-- entries : Grid -> Square -> List Entry
--
-- type alias Entry =
--     { startingCoordinates : Coordinates
--     , direction : Direction
--     , word : String
--     , clue : String
--     }
--
--
-- type alias Puzzle =
--     { entries : List Entry
--     }
