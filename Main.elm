module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Grid =
    List Square


type alias Letter =
    Char


type Square
    = Open Coordinates Letter
    | Filled Coordinates


initGrid : Grid
initGrid =
    [ Open { x = 0, y = 0 } 'A'
    , Open { x = 1, y = 0 } 'B'
    , Open { x = 2, y = 0 } 'C'
    , Open { x = 0, y = 1 } 'D'
    , Filled { x = 1, y = 1 }
    , Open { x = 2, y = 1 } 'E'
    , Open { x = 0, y = 2 } 'F'
    , Open { x = 1, y = 2 } 'G'
    , Open { x = 2, y = 2 } 'H'
    ]


getSquareByCoordinates : Grid -> Coordinates -> Maybe Square
getSquareByCoordinates grid coordinates =
    List.filter
        (\square ->
            coordinatesEqual (squareCoordinates square) coordinates
        )
        grid
        |> List.head


coordinatesEqual : Coordinates -> Coordinates -> Bool
coordinatesEqual coords1 coords2 =
    coords1 == coords2


squareCoordinates : Square -> Coordinates
squareCoordinates square =
    case square of
        Open coords _ ->
            coords

        Filled coords ->
            coords


type alias Coordinates =
    { x : Int
    , y : Int
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
    ( { grid = initGrid }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- placeholder view code for now


squareView : Square -> Html Msg
squareView square =
    case square of
        Open coords letter ->
            div
                [ class "square--open"
                , style
                    [ ( "width", "32px" )
                    , ( "height", "32px" )
                    , ( "display", "inline-block" )
                    , ( "box-sizing", "border-box" )
                    , ( "vertical-align", "top" )
                    , ( "padding", "8px 0" )
                    , ( "text-align", "center" )
                    , ( "border-right", "1px solid gray" )
                    , ( "border-bottom", "1px solid gray" )
                    ]
                ]
                [ text <| String.fromChar letter ]

        Filled coords ->
            div
                [ class "square--filled"
                , style
                    [ ( "width", "32px" )
                    , ( "height", "32px" )
                    , ( "display", "inline-block" )
                    , ( "background-color", "black" )
                    , ( "box-sizing", "border-box" )
                    , ( "vertical-align", "top" )
                    ]
                ]
                []


gridToRows : Grid -> List (List Square)
gridToRows grid =
    let
        hasSameY square1 square2 =
            (squareCoordinates square1).y == (squareCoordinates square2).y
    in
        grid
            |> List.sortBy (squareCoordinates >> .y)
            |> List.Extra.groupWhile hasSameY


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "inline-block" )
            , ( "border-left", "1px solid gray" )
            , ( "border-top", "1px solid gray" )
            ]
        ]
    <|
        List.map
            (List.map squareView >> div [])
            (gridToRows model.grid)
