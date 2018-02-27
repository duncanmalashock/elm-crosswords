module Grid
    exposing
        ( Grid
        , Square(..)
        , initGrid
        , fromString
        , view
        , squareCoordinates
        , isAcrossEntryStart
        , isDownEntryStart
        )

import Coordinates exposing (Coordinates)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra


type alias Grid =
    List Square


type alias Letter =
    Char


type Square
    = LetterSquare Coordinates Letter
    | BlockSquare Coordinates


fromString : Int -> Int -> String -> Result String Grid
fromString gridWidth gridHeight string =
    let
        charList =
            string
                |> String.toList
    in
        charsToSquares gridWidth gridHeight ( 0, 0 ) charList


charsToSquares : Int -> Int -> Coordinates -> List Char -> Result String (List Square)
charsToSquares gridWidth gridHeight ( curX, curY ) charList =
    case charList of
        [] ->
            Ok []

        head :: tail ->
            let
                ( newX, newY ) =
                    if curX >= gridWidth then
                        ( 0, curY + 1 )
                    else
                        ( curX, curY )
            in
                Result.map2 (::)
                    (charToSquare head ( newX, newY ))
                    (charsToSquares gridWidth gridHeight ( newX + 1, newY ) tail)


charToSquare : Char -> Coordinates -> Result String Square
charToSquare char coords =
    case char of
        '.' ->
            Ok <| LetterSquare coords ' '

        '*' ->
            Ok <| BlockSquare coords

        _ ->
            Err "Invalid characters"


initGrid : Grid
initGrid =
    fromString 3 3 "..**.*.*."
        |> Result.withDefault []


gridToRows : Grid -> List (List Square)
gridToRows grid =
    let
        hasSameY square1 square2 =
            squareYCoordinate square1 == squareYCoordinate square2
    in
        grid
            |> List.sortBy (squareCoordinates >> Coordinates.yCoordinate)
            |> List.Extra.groupWhile hasSameY


view : Grid -> Html msg
view grid =
    let
        sortRow : List Square -> List Square
        sortRow squares =
            List.sortBy (\s -> squareXCoordinate s) squares

        drawRow : List Square -> Html msg
        drawRow squares =
            div []
                (List.map (squareView grid) squares)
    in
        (div
            [ style
                [ ( "display", "inline-block" )
                , ( "border-left", "1px solid gray" )
                , ( "border-top", "1px solid gray" )
                ]
            ]
        )
            (grid
                |> gridToRows
                |> List.map (sortRow >> drawRow)
            )


isAcrossEntryStart : Grid -> Square -> Bool
isAcrossEntryStart grid square =
    (not <| hasLetterSquareSquareAtLeft grid square)


isDownEntryStart : Grid -> Square -> Bool
isDownEntryStart grid square =
    (not <| hasLetterSquareSquareAbove grid square)


squareView : Grid -> Square -> Html msg
squareView grid square =
    case square of
        LetterSquare coords letter ->
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
                [ text "" ]

        BlockSquare coords ->
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


blankSquare : Coordinates -> Square
blankSquare coordinates =
    LetterSquare coordinates ' '


blockSquare : Coordinates -> Square
blockSquare coordinates =
    BlockSquare coordinates


squareCoordinates : Square -> Coordinates
squareCoordinates square =
    case square of
        LetterSquare coords _ ->
            coords

        BlockSquare coords ->
            coords


squareXCoordinate : Square -> Int
squareXCoordinate square =
    Coordinates.xCoordinate <| squareCoordinates square


squareYCoordinate : Square -> Int
squareYCoordinate square =
    Coordinates.yCoordinate <| squareCoordinates square


squareIsAtCoordinates : Square -> Coordinates -> Bool
squareIsAtCoordinates square coordinates =
    (squareCoordinates square == coordinates)


squareAtCoordinates : Grid -> Coordinates -> Maybe Square
squareAtCoordinates grid coordinates =
    grid
        |> List.filter (\square -> squareIsAtCoordinates square coordinates)
        |> List.head


squareIsLetterSquare : Square -> Bool
squareIsLetterSquare square =
    case square of
        LetterSquare _ _ ->
            True

        BlockSquare _ ->
            False


squareAbove : Grid -> Square -> Maybe Square
squareAbove grid square =
    squareAtCoordinates grid <| Coordinates.above <| squareCoordinates square


squareAtLeft : Grid -> Square -> Maybe Square
squareAtLeft grid square =
    squareAtCoordinates grid <| Coordinates.atLeft <| squareCoordinates square


hasLetterSquareSquareAbove : Grid -> Square -> Bool
hasLetterSquareSquareAbove grid square =
    squareAbove grid square
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False


hasLetterSquareSquareAtLeft : Grid -> Square -> Bool
hasLetterSquareSquareAtLeft grid square =
    squareAtLeft grid square
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False
