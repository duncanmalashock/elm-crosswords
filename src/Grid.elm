module Grid
    exposing
        ( Grid
        , Square(..)
        , StartsEntries(..)
        , width
        , height
        , blank
        , fromString
        , toRows
        , clear
        , toString
        , isAcrossEntryStart
        , isDownEntryStart
        , coordIsInBounds
        , squareAtCoordinate
        , hasLetterSquareAt
        , updateLetterSquare
        )

import Char
import Maybe.Extra as Maybe
import Direction exposing (Direction(..))
import Coordinate exposing (Coordinate)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import List.Extra
import Matrix exposing (Matrix)
import Array.Hamt as Array exposing (Array)


type alias Grid =
    Matrix Square


type StartsEntries
    = StartsAcross
    | StartsDown
    | StartsAcrossAndDown
    | NoStart


type alias EntryData =
    { startsEntries : StartsEntries
    , inAcrossEntry : Int
    , inDownEntry : Int
    }


type Square
    = LetterSquare Coordinate Char EntryData
    | BlockSquare Coordinate


blankEntryData =
    { startsEntries = NoStart
    , inAcrossEntry = 1
    , inDownEntry = 1
    }


width : Grid -> Int
width grid =
    Matrix.width grid


height : Grid -> Int
height grid =
    Matrix.height grid


blank : Int -> Int -> Result String Grid
blank gridWidth gridHeight =
    let
        initString =
            String.repeat (gridWidth * gridHeight) "."
    in
        fromString gridWidth gridHeight initString


fromString : Int -> Int -> String -> Result String Grid
fromString gridWidth gridHeight string =
    let
        input =
            string
                |> String.toList
                |> checkLength ( gridWidth, gridHeight )

        startingGrid =
            Matrix.repeat gridWidth gridHeight (blankSquare ( 0, 0 ) blankEntryData)
                |> Ok
    in
        case input of
            Ok charList ->
                fromStringHelp gridWidth gridHeight ( 0, 0 ) charList 0 startingGrid

            Err error ->
                Err error


fromStringHelp : Int -> Int -> Coordinate -> List Char -> Int -> Result String Grid -> Result String Grid
fromStringHelp gridWidth gridHeight ( curX, curY ) charList entryNumberSoFar gridSoFar =
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

                newEntryNumber =
                    if
                        resultToBool (Result.map (isAcrossEntryStart ( newX, newY )) gridSoFar)
                            || resultToBool (Result.map (isDownEntryStart ( newX, newY )) gridSoFar)
                    then
                        entryNumberSoFar + 1
                    else
                        entryNumberSoFar
            in
                Result.map2 (Matrix.set newX newY)
                    (charToSquare head ( newX, newY ) newEntryNumber gridSoFar)
                    (fromStringHelp gridWidth gridHeight ( newX + 1, newY ) tail newEntryNumber gridSoFar)


getEntryData : Grid -> Coordinate -> Int -> EntryData
getEntryData grid ( x, y ) newEntryNumber =
    let
        acrossEntryNumber =
            if isAcrossEntryStart ( x, y ) grid then
                newEntryNumber
            else
                findAcrossEntryStartNumber grid ( x, y )
                    |> Maybe.withDefault -1

        downEntryNumber =
            if isDownEntryStart ( x, y ) grid then
                newEntryNumber
            else
                findDownEntryStartNumber grid ( x, y )
                    |> Maybe.withDefault -1

        startsEntriesValue =
            if (isAcrossEntryStart ( x, y ) grid) && (isDownEntryStart ( x, y ) grid) then
                StartsAcrossAndDown
            else if (isAcrossEntryStart ( x, y ) grid) then
                StartsAcross
            else if (isDownEntryStart ( x, y ) grid) then
                StartsDown
            else
                NoStart
    in
        { startsEntries = startsEntriesValue
        , inAcrossEntry = acrossEntryNumber
        , inDownEntry = downEntryNumber
        }


toString : Grid -> String
toString grid =
    toRows grid
        |> toStringHelp


toStringHelp : List (List ( Coordinate, Square )) -> String
toStringHelp rows =
    List.map rowToString rows
        |> String.join "\n"


resultToBool : Result String Bool -> Bool
resultToBool result =
    Result.withDefault False result


rowToString : List ( Coordinate, Square ) -> String
rowToString row =
    let
        squares =
            List.map (\( _, s ) -> s) row
    in
        List.map squareToString squares
            |> String.join ""


squareToString : Square -> String
squareToString s =
    case s of
        LetterSquare _ c _ ->
            String.fromChar c

        BlockSquare _ ->
            "*"


charToSquare : Char -> Coordinate -> Int -> Result String Grid -> Result String Square
charToSquare char ( x, y ) entryNumber gridResult =
    case gridResult of
        Ok grid ->
            if List.member (Char.toUpper char) (String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ") then
                Ok <|
                    letterSquare ( x, y ) (Char.toUpper char) (getEntryData grid ( x, y ) entryNumber)
            else if char == '.' then
                Ok <|
                    blankSquare ( x, y ) (getEntryData grid ( x, y ) entryNumber)
            else if char == '*' then
                Ok (blockSquare ( x, y ))
            else
                Err <| "Invalid character: " ++ (String.fromChar char)

        Err _ ->
            Err "Invalid grid"


findAcrossEntryStartNumber : Grid -> Coordinate -> Maybe Int
findAcrossEntryStartNumber grid ( x, y ) =
    case isAcrossEntryStart ( x, y ) grid of
        True ->
            squareAtCoordinate grid ( x, y )
                |> Maybe.map acrossEntryNumber
                |> Maybe.join

        False ->
            findAcrossEntryStartNumber grid (Coordinate.atLeft ( x, y ))


findDownEntryStartNumber : Grid -> Coordinate -> Maybe Int
findDownEntryStartNumber grid ( x, y ) =
    case isDownEntryStart ( x, y ) grid of
        True ->
            squareAtCoordinate grid ( x, y )
                |> Maybe.map downEntryNumber
                |> Maybe.join

        False ->
            findDownEntryStartNumber grid (Coordinate.above ( x, y ))


acrossEntryNumber : Square -> Maybe Int
acrossEntryNumber square =
    case square of
        LetterSquare _ _ entryData ->
            Just entryData.inAcrossEntry

        BlockSquare _ ->
            Nothing


downEntryNumber : Square -> Maybe Int
downEntryNumber square =
    case square of
        LetterSquare _ _ entryData ->
            Just entryData.inDownEntry

        BlockSquare _ ->
            Nothing


updateLetterSquare : Coordinate -> Char -> Grid -> Grid
updateLetterSquare ( x, y ) char grid =
    Matrix.set x y (letterSquare ( x, y ) (Char.toUpper char) blankEntryData) grid


clear : Grid -> Grid
clear grid =
    Matrix.map
        (\s ->
            case s of
                LetterSquare _ _ _ ->
                    blankSquare ( 0, 0 ) blankEntryData

                BlockSquare _ ->
                    blockSquare ( 0, 0 )
        )
        grid


toRows : Grid -> List (List ( Coordinate, Square ))
toRows grid =
    let
        gridYIndices =
            (List.range 0 (Matrix.height grid - 1))

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
        [ (Basics.toString length)
        , " is too "
        , fewOrMany
        , " characters for a "
        , (Basics.toString width)
        , "x"
        , (Basics.toString height)
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


isAcrossEntryStart : Coordinate -> Grid -> Bool
isAcrossEntryStart coordinate grid =
    (not <| hasLetterSquareAtLeft grid coordinate)


isDownEntryStart : Coordinate -> Grid -> Bool
isDownEntryStart coordinate grid =
    (not <| hasLetterSquareAbove grid coordinate)


letterSquare : Coordinate -> Char -> EntryData -> Square
letterSquare coord char entryData =
    LetterSquare coord char entryData


blankSquare : Coordinate -> EntryData -> Square
blankSquare coord entryData =
    LetterSquare coord ' ' entryData


blockSquare : Coordinate -> Square
blockSquare coord =
    BlockSquare coord


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
        LetterSquare _ _ _ ->
            True

        BlockSquare _ ->
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
