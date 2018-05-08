module Grid
    exposing
        ( Grid
        , Square(..)
        , width
        , height
        , flatten
        , blank
        , fromString
        , toRows
        , clear
        , isAcrossEntryStart
        , isDownEntryStart
        , coordIsInBounds
        , squareAtCoordinate
        , hasLetterSquareAt
        , updateLetterSquare
        )

import Char
import Direction exposing (Direction(..))
import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


type alias Grid =
    Matrix Square


type alias Entry =
    { number : Int
    , direction : Direction
    }


type StartsEntries
    = StartsAcross
    | StartsDown
    | StartsAcrossAndDown
    | NoStart


type alias EntryData =
    { startsEntries : StartsEntries
    , inAcrossEntry : Entry
    , inDownEntry : Entry
    }


type Square
    = LetterSquare Char EntryData
    | BlockSquare


blankEntryData =
    let
        acrossEntry =
            { number = 0
            , direction = Across
            }

        downEntry =
            { number = 0
            , direction = Down
            }
    in
        { startsEntries = NoStart
        , inAcrossEntry = acrossEntry
        , inDownEntry = downEntry
        }


width : Grid -> Int
width grid =
    Matrix.width grid


height : Grid -> Int
height grid =
    Matrix.height grid


flatten : Grid -> List Square
flatten grid =
    grid.data
        |> Array.toList


blank : Int -> Int -> Grid
blank gridWidth gridHeight =
    Matrix.repeat gridWidth gridHeight (blankSquare blankEntryData)


fromString : Int -> Int -> String -> Result String Grid
fromString gridWidth gridHeight string =
    let
        input =
            string
                |> String.toList
                |> checkLength ( gridWidth, gridHeight )

        startingGrid =
            Matrix.repeat gridWidth gridHeight (blankSquare blankEntryData)
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
        Ok <| letterSquare (Char.toUpper char) blankEntryData
    else if char == '.' then
        Ok (blankSquare blankEntryData)
    else if char == '*' then
        Ok blockSquare
    else
        Err "Invalid character"


updateLetterSquare : Coordinate -> Char -> Grid -> Grid
updateLetterSquare ( x, y ) char grid =
    Matrix.set x y (letterSquare (Char.toUpper char) blankEntryData) grid


clear : Grid -> Grid
clear grid =
    Matrix.map
        (\s ->
            case s of
                LetterSquare _ _ ->
                    blankSquare blankEntryData

                BlockSquare ->
                    blockSquare
        )
        grid


toRows : Grid -> List (List ( Coordinate, Square ))
toRows grid =
    let
        gridYIndices =
            (List.range 0 (Matrix.height grid))

        getIndexedWithYIndexBefore y =
            ( y, Array.toIndexedList (getRow grid y) )

        moveYIndexIntoCoordinate =
            List.map
                (\( y, ss ) ->
                    List.map (\( x, s ) -> ( ( x, y ), s )) ss
                )
    in
        List.map getIndexedWithYIndexBefore gridYIndices
            |> moveYIndexIntoCoordinate


getRow : Grid -> Int -> Array Square
getRow grid index =
    Matrix.getRow index grid
        |> Maybe.withDefault Array.empty


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


isAcrossEntryStart : Grid -> Coordinate -> Bool
isAcrossEntryStart grid coordinate =
    (not <| hasLetterSquareAtLeft grid coordinate)


isDownEntryStart : Grid -> Coordinate -> Bool
isDownEntryStart grid coordinate =
    (not <| hasLetterSquareAbove grid coordinate)


letterSquare : Char -> EntryData -> Square
letterSquare char entryData =
    LetterSquare char entryData


blankSquare : EntryData -> Square
blankSquare entryData =
    LetterSquare ' ' entryData


blockSquare : Square
blockSquare =
    BlockSquare


coordIsInBounds : Grid -> Coordinate -> Bool
coordIsInBounds grid ( x, y ) =
    let
        isInXBounds xVal =
            xVal >= 0 && xVal < (width grid)

        isInYBounds yVal =
            yVal >= 0 && yVal < (height grid)
    in
        (isInXBounds x) && (isInYBounds y)


squareAtCoordinate : Grid -> Coordinate -> Maybe Square
squareAtCoordinate grid ( x, y ) =
    Matrix.get x y grid


squareIsLetterSquare : Square -> Bool
squareIsLetterSquare square =
    case square of
        LetterSquare _ _ ->
            True

        BlockSquare ->
            False


squareAbove : Grid -> Coordinate -> Maybe Square
squareAbove grid coordinate =
    squareAtCoordinate grid <| Coordinate.above coordinate


squareAtLeft : Grid -> Coordinate -> Maybe Square
squareAtLeft grid coordinate =
    squareAtCoordinate grid <| Coordinate.atLeft coordinate


hasLetterSquareAt : Grid -> Coordinate -> Bool
hasLetterSquareAt grid coordinate =
    squareAtCoordinate grid coordinate
        |> Maybe.map squareIsLetterSquare
        |> Maybe.withDefault False


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
