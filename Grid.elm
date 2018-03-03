module Grid
    exposing
        ( Grid
        , Square(..)
        , flatten
        , empty
        , fromString
        , view
        , squareCoordinate
        , isAcrossEntryStart
        , isDownEntryStart
        )

import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Matrix exposing (Matrix)
import Array.Hamt as Array


type alias Grid =
    Matrix Square


type Square
    = LetterSquare Coordinate Char
    | BlockSquare Coordinate


flatten : Grid -> List Square
flatten grid =
    grid.data
        |> Array.toList


empty : Grid
empty =
    Matrix.empty


fromString : Int -> Int -> String -> Result String Grid
fromString gridWidth gridHeight string =
    let
        input =
            string
                |> String.toList
                |> checkLength ( gridWidth, gridHeight )

        startingGrid =
            Matrix.repeat gridWidth gridHeight (blankSquare ( 0, 0 ))
                |> Ok
    in
        case input of
            Ok charList ->
                fromStringHelp gridWidth gridHeight ( 0, 0 ) charList startingGrid

            Err error ->
                Err error


fromStringHelp : Int -> Int -> Coordinate -> List Char -> Result String (Matrix Square) -> Result String (Matrix Square)
fromStringHelp gridWidth gridHeight ( curX, curY ) charList gridSoFar =
    case charList of
        [] ->
            gridSoFar

        head :: tail ->
            let
                ( newX, newY ) =
                    if curX >= gridWidth then
                        ( 0, curY + 1 )
                    else
                        ( curX, curY )
            in
                Result.map2 (Matrix.set newX newY)
                    (charToSquare head ( newX, newY ))
                    (fromStringHelp gridWidth gridHeight ( newX + 1, newY ) tail gridSoFar)


charToSquare : Char -> Coordinate -> Result String Square
charToSquare char coords =
    case char of
        '.' ->
            Ok <| LetterSquare coords ' '

        '*' ->
            Ok <| BlockSquare coords

        _ ->
            Err "Invalid character"


gridToRows : Grid -> List (List Square)
gridToRows grid =
    let
        hasSameY square1 square2 =
            squareYCoordinate square1 == squareYCoordinate square2
    in
        grid
            |> flatten
            |> List.sortBy (squareCoordinate >> Coordinate.yCoordinate)
            |> List.Extra.groupWhile hasSameY


lengthMismatchError : Int -> String -> ( Int, Int ) -> String
lengthMismatchError length fewOrMany ( width, height ) =
    String.concat
        [ (toString length)
        , " is too "
        , fewOrMany
        , " characters for a "
        , (toString width)
        , "x"
        , (toString height)
        , " Grid"
        ]


checkLength : ( Int, Int ) -> List Char -> Result String (List Char)
checkLength ( gridWidth, gridHeight ) charList =
    let
        expectedLength =
            gridWidth * gridHeight

        actualLength =
            List.length charList
    in
        if actualLength == expectedLength then
            Ok charList
        else if actualLength < expectedLength then
            Err <| lengthMismatchError actualLength "few" ( gridWidth, gridHeight )
        else
            Err <| lengthMismatchError actualLength "many" ( gridWidth, gridHeight )


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
    (not <| hasLetterSquareAtLeft grid square)


isDownEntryStart : Grid -> Square -> Bool
isDownEntryStart grid square =
    (not <| hasLetterSquareAbove grid square)


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


blankSquare : Coordinate -> Square
blankSquare coordinates =
    LetterSquare coordinates ' '


blockSquare : Coordinate -> Square
blockSquare coordinates =
    BlockSquare coordinates


squareCoordinate : Square -> Coordinate
squareCoordinate square =
    case square of
        LetterSquare coords _ ->
            coords

        BlockSquare coords ->
            coords


squareXCoordinate : Square -> Int
squareXCoordinate square =
    Coordinate.xCoordinate <| squareCoordinate square


squareYCoordinate : Square -> Int
squareYCoordinate square =
    Coordinate.yCoordinate <| squareCoordinate square


squareIsAtCoordinate : Square -> Coordinate -> Bool
squareIsAtCoordinate square coordinates =
    (squareCoordinate square == coordinates)


squareAtCoordinate : Grid -> Coordinate -> Maybe Square
squareAtCoordinate grid coordinates =
    grid
        |> flatten
        |> List.filter (\square -> squareIsAtCoordinate square coordinates)
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
    squareAtCoordinate grid <| Coordinate.above <| squareCoordinate square


squareAtLeft : Grid -> Square -> Maybe Square
squareAtLeft grid square =
    squareAtCoordinate grid <| Coordinate.atLeft <| squareCoordinate square


hasLetterSquareAbove : Grid -> Square -> Bool
hasLetterSquareAbove grid square =
    squareAbove grid square
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False


hasLetterSquareAtLeft : Grid -> Square -> Bool
hasLetterSquareAtLeft grid square =
    squareAtLeft grid square
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False
