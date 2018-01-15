module Main exposing (main)

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
    { grid : Grid
    }


type alias Grid =
    List Square


type Selected
    = Selected
    | NotSelected


type alias Letter =
    Char


type alias Id =
    Int


type Square
    = Open Id Selected Letter Neighbors
    | Filled Id Neighbors


type NeighborDirection
    = Left
    | Right
    | Up
    | Down


initGrid : Grid
initGrid =
    [ Open 0 NotSelected 'a' { up = Nothing, down = Just 3, right = Just 1, left = Nothing }
    , Open 1 NotSelected 'a' { up = Nothing, down = Just 4, right = Just 2, left = Just 0 }
    , Open 2 NotSelected 'a' { up = Nothing, down = Just 5, right = Nothing, left = Just 1 }
    , Open 3 NotSelected 'a' { up = Just 0, down = Just 6, right = Just 4, left = Nothing }
    , Filled 4 { up = Just 1, down = Just 7, right = Just 5, left = Just 3 }
    , Open 5 NotSelected 'a' { up = Just 2, down = Just 8, right = Nothing, left = Just 4 }
    , Open 6 NotSelected 'a' { up = Just 3, down = Nothing, right = Just 7, left = Nothing }
    , Open 7 NotSelected 'a' { up = Just 4, down = Nothing, right = Just 8, left = Just 6 }
    , Open 8 NotSelected 'a' { up = Just 5, down = Nothing, right = Nothing, left = Just 7 }
    ]


getNeigbor : Grid -> Square -> (Neighbors -> Maybe Id) -> Maybe Square
getNeigbor grid square accessor =
    square
        |> neighbors
        |> accessor
        |> Maybe.andThen (getSquareById grid)


upNeighbor : Grid -> Square -> Maybe Square
upNeighbor grid square =
    getNeigbor grid square .up


downNeighbor : Grid -> Square -> Maybe Square
downNeighbor grid square =
    getNeigbor grid square .down


leftNeighbor : Grid -> Square -> Maybe Square
leftNeighbor grid square =
    getNeigbor grid square .left


rightNeighbor : Grid -> Square -> Maybe Square
rightNeighbor grid square =
    getNeigbor grid square .right


getSquareById : Grid -> Id -> Maybe Square
getSquareById grid id =
    List.filter
        (\square ->
            idIs id (squareId square)
        )
        grid
        |> List.head


idIs : Id -> Id -> Bool
idIs id1 id2 =
    id1 == id2


squareId : Square -> Id
squareId square =
    case square of
        Open id _ _ _ ->
            id

        Filled id _ ->
            id


neighbors : Square -> Neighbors
neighbors square =
    case square of
        Open _ _ _ neighbors ->
            neighbors

        Filled _ neighbors ->
            neighbors


type alias Neighbors =
    { up : Maybe Id
    , down : Maybe Id
    , right : Maybe Id
    , left : Maybe Id
    }


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
    ( { puzzle = { grid = initGrid } }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


squareView : Square -> Html Msg
squareView square =
    let
        squareContent square =
            case square of
                Open id _ letter _ ->
                    "[" ++ toString letter ++ "]"

                Filled id _ ->
                    "[#]"
    in
        div []
            [ text <| squareContent square ]


newPlacement : ( Int, Int ) -> NeighborDirection -> ( Int, Int )
newPlacement ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 10 )

        Down ->
            ( x, y + 10 )

        Left ->
            ( x - 10, y )

        Right ->
            ( x + 10, y )


view : Model -> Html Msg
view model =
    div []
        (List.map squareView model.puzzle.grid)
