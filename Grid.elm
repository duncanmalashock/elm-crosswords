module Grid
    exposing
        ( Grid
        , Square(..)
        , flatten
        , empty
        , fromString
        , view
        , isAcrossEntryStart
        , isDownEntryStart
        , squareAtCoordinate
        , squareAtRight
        , squareBelow
        )

import Char
import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Matrix exposing (Matrix)
import Array.Hamt as Array


type alias Grid =
    Matrix Square


type Square
    = LetterSquare Char
    | BlockSquare


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
            Matrix.repeat gridWidth gridHeight blankSquare
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
                    (charToSquare head)
                    (fromStringHelp gridWidth gridHeight ( newX + 1, newY ) tail gridSoFar)


charToSquare : Char -> Result String Square
charToSquare char =
    if List.member (Char.toUpper char) (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ") then
        Ok <| LetterSquare (Char.toUpper char)
    else if char == '.' then
        Ok blankSquare
    else if char == '*' then
        Ok blockSquare
    else
        Err "Invalid character"


gridToRows : Grid -> List (List Square)
gridToRows grid =
    List.map (\r -> Matrix.getRow r grid) (List.range 0 (Matrix.height grid))
        |> List.map (Maybe.withDefault Array.empty >> Array.toList)


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
                |> List.map (drawRow)
            )


isAcrossEntryStart : Grid -> Coordinate -> Bool
isAcrossEntryStart grid coordinate =
    (not <| hasLetterSquareAtLeft grid coordinate)


isDownEntryStart : Grid -> Coordinate -> Bool
isDownEntryStart grid coordinate =
    (not <| hasLetterSquareAbove grid coordinate)


squareView : Grid -> Square -> Html msg
squareView grid square =
    case square of
        LetterSquare letter ->
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

        BlockSquare ->
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


blankSquare : Square
blankSquare =
    LetterSquare ' '


blockSquare : Square
blockSquare =
    BlockSquare


squareAtCoordinate : Grid -> Coordinate -> Maybe Square
squareAtCoordinate grid ( x, y ) =
    Matrix.get x y grid


squareIsLetterSquare : Square -> Bool
squareIsLetterSquare square =
    case square of
        LetterSquare _ ->
            True

        BlockSquare ->
            False


squareAbove : Grid -> Coordinate -> Maybe Square
squareAbove grid coordinate =
    squareAtCoordinate grid <| Coordinate.above coordinate


squareBelow : Grid -> Coordinate -> Maybe Square
squareBelow grid coordinate =
    squareAtCoordinate grid <| Coordinate.below coordinate


squareAtLeft : Grid -> Coordinate -> Maybe Square
squareAtLeft grid coordinate =
    squareAtCoordinate grid <| Coordinate.atLeft coordinate


squareAtRight : Grid -> Coordinate -> Maybe Square
squareAtRight grid coordinate =
    squareAtCoordinate grid <| Coordinate.atRight coordinate


hasLetterSquareAbove : Grid -> Coordinate -> Bool
hasLetterSquareAbove grid coordinate =
    squareAbove grid coordinate
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False


hasLetterSquareAtLeft : Grid -> Coordinate -> Bool
hasLetterSquareAtLeft grid coordinate =
    squareAtLeft grid coordinate
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False
