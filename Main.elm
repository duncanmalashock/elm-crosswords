module Main exposing (main)

import Views
import Coordinate exposing (Coordinate)
import Grid exposing (Grid)
import Entry exposing (EntryListings)
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
    { grid : Result String Grid
    , entryListings : EntryListings
    }


type Msg
    = SetSquare Coordinate


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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

        grid =
            Grid.fromString 15 15 stringInput
    in
        ( { grid = grid
          , entryListings =
                Entry.allFromGrid
                    (Result.withDefault Grid.empty grid)
          }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSquare coord ->
            let
                updatedGrid =
                    Result.map (Grid.setAtCoordinate coord) model.grid
            in
                ( { model
                    | grid = updatedGrid
                    , entryListings =
                        Entry.allFromGrid
                            (updatedGrid |> Result.withDefault Grid.empty)
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div []
        [ Views.gridView (model.grid |> Result.withDefault Grid.empty) SetSquare
        , Views.entriesView model.entryListings
        ]
