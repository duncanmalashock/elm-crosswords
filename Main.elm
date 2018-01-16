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


blankSquare : Coordinates -> Square
blankSquare coordinates =
    Open coordinates ' '


filledSquare : Coordinates -> Square
filledSquare coordinates =
    Filled coordinates


blankSquares : Coordinates -> Int -> List Square
blankSquares ( x, y ) number =
    if number <= 0 then
        []
    else
        blankSquare ( x, y ) :: blankSquares ( x + 1, y ) (number - 1)


filledSquares : Coordinates -> Int -> List Square
filledSquares ( x, y ) number =
    if number <= 0 then
        []
    else
        filledSquare ( x, y ) :: filledSquares ( x + 1, y ) (number - 1)


initGrid : Grid
initGrid =
    createGridWithRotationalSymmetry
        [ [ 6, 1, 3, 1, 4 ]
        , [ 6, 1, 3, 1, 4 ]
        , [ 6, 1, 8 ]
        , [ 4, 1, 3, 2, 5 ]
        , [ 3, 1, 7, 1, 3 ]
        , [ 7, 1, 6, 1, 0 ]
        , [ 5, 2, 5, 3, 0 ]
        , [ 0, 1, 3, 1, 5, 1, 3, 1, 0 ]
        ]


createGridWithRotationalSymmetry : List (List Int) -> Grid
createGridWithRotationalSymmetry gridSpec =
    let
        flipGridSpec spec =
            spec
                |> List.map (List.reverse)
                |> List.reverse
                |> List.drop 1
    in
        doCreateGrid (gridSpec ++ flipGridSpec gridSpec)


doCreateGrid : List (List Int) -> Grid
doCreateGrid gridSpec =
    createGrid gridSpec [] 0
        |> List.concat
        |> List.concat


createGrid : List (List Int) -> Grid -> Int -> List (List Grid)
createGrid lists grid index =
    case lists of
        [] ->
            []

        intList :: intLists ->
            gridRow intList [] ( 0, index ) :: (createGrid intLists grid (index + 1))


gridRow : List Int -> Grid -> Coordinates -> List Grid
gridRow squareLists grid ( x, y ) =
    case squareLists of
        [] ->
            []

        int :: ints ->
            if List.length ints % 2 == 0 then
                blankSquares ( x, y ) int :: gridRow ints grid ( x + int, y )
            else
                filledSquares ( x, y ) int :: gridRow ints grid ( x + int, y )


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


xCoordinate : Coordinates -> Int
xCoordinate ( x, _ ) =
    x


yCoordinate : Coordinates -> Int
yCoordinate ( _, y ) =
    y


squareXCoordinate : Square -> Int
squareXCoordinate square =
    xCoordinate <| squareCoordinates square


squareYCoordinate : Square -> Int
squareYCoordinate square =
    yCoordinate <| squareCoordinates square


type alias Coordinates =
    ( Int, Int )


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
            squareYCoordinate square1 == squareYCoordinate square2
    in
        grid
            |> List.sortBy (squareCoordinates >> yCoordinate)
            |> List.Extra.groupWhile hasSameY


view : Model -> Html Msg
view model =
    let
        sortRow : List Square -> List Square
        sortRow squares =
            List.sortBy (\s -> squareXCoordinate s) squares

        drawRow : List Square -> Html Msg
        drawRow squares =
            div []
                (List.map (squareView) squares)
    in
        (div
            [ style
                [ ( "display", "inline-block" )
                , ( "border-left", "1px solid gray" )
                , ( "border-top", "1px solid gray" )
                ]
            ]
        )
            (model.grid
                |> gridToRows
                |> List.map (sortRow >> drawRow)
            )
