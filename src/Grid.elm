module Grid
    exposing
        ( Grid
        , width
        , height
        , blank
        , fromString
        , toRows
        , toString
        , isAcrossEntryStart
        , isDownEntryStart
        , coordIsInBounds
        , squareAtCoordinate
        , hasLetterSquareAt
        , setGuess
        , acrossClues
        , downClues
        )

import Char
import Square exposing (Square(..), EntryData, StartsEntries(..))
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


blankEntryData =
    { startsEntries = NoStart
    , inAcrossEntry = -1
    , inDownEntry = -1
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
            String.fromChar Square.blankChar
                |> String.repeat (gridWidth * gridHeight)
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
                        (resultToBool (Result.map (isAcrossEntryStart ( newX, newY )) gridSoFar)
                            || resultToBool (Result.map (isDownEntryStart ( newX, newY )) gridSoFar)
                        )
                            && Square.isLetterChar head
                    then
                        entryNumberSoFar + 1
                    else
                        entryNumberSoFar

                newGrid : Result String Grid
                newGrid =
                    Result.map2
                        (Matrix.set newX newY)
                        (charToSquare head ( newX, newY ) newEntryNumber gridSoFar)
                        gridSoFar
            in
                fromStringHelp gridWidth gridHeight ( newX + 1, newY ) tail newEntryNumber newGrid


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
                StartsAcrossAndDown { clue = "" } { clue = "" }
            else if (isAcrossEntryStart ( x, y ) grid) then
                StartsAcross { clue = "" }
            else if (isDownEntryStart ( x, y ) grid) then
                StartsDown { clue = "" }
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
        List.map Square.toString squares
            |> String.join ""


charToSquare : Char -> Coordinate -> Int -> Result String Grid -> Result String Square
charToSquare char ( x, y ) entryNumber gridResult =
    case gridResult of
        Ok grid ->
            if char == Square.blankChar then
                Ok <|
                    blankSquare ( x, y ) (getEntryData grid ( x, y ) entryNumber)
            else if Square.isLetterChar char then
                Ok <|
                    letterSquare ( x, y ) (Char.toUpper char) (getEntryData grid ( x, y ) entryNumber)
            else if char == Square.blockChar then
                Ok (blockSquare ( x, y ))
            else
                Err <| "Invalid character: " ++ (String.fromChar char)

        Err message ->
            Err message


findAcrossEntryStartNumber : Grid -> Coordinate -> Maybe Int
findAcrossEntryStartNumber grid ( x, y ) =
    case isAcrossEntryStart ( x, y ) grid of
        True ->
            squareAtCoordinate grid ( x, y )
                |> Maybe.map findAcrossEntryStartNumberHelp
                |> Maybe.join

        False ->
            findAcrossEntryStartNumber grid (Coordinate.atLeft ( x, y ))


findAcrossEntryStartNumberHelp : Square -> Maybe Int
findAcrossEntryStartNumberHelp square =
    case square of
        LetterSquare _ _ entryData ->
            Just entryData.inAcrossEntry

        BlockSquare _ ->
            Nothing


findDownEntryStartNumber : Grid -> Coordinate -> Maybe Int
findDownEntryStartNumber grid ( x, y ) =
    case isDownEntryStart ( x, y ) grid of
        True ->
            squareAtCoordinate grid ( x, y )
                |> Maybe.map findDownEntryStartNumberHelp
                |> Maybe.join

        False ->
            findDownEntryStartNumber grid (Coordinate.above ( x, y ))


findDownEntryStartNumberHelp : Square -> Maybe Int
findDownEntryStartNumberHelp square =
    case square of
        LetterSquare _ _ entryData ->
            Just entryData.inDownEntry

        BlockSquare _ ->
            Nothing


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
    LetterSquare
        coord
        { solution = char
        , guess = ' '
        }
        entryData


blankSquare : Coordinate -> EntryData -> Square
blankSquare coord entryData =
    LetterSquare
        coord
        { solution = ' '
        , guess = ' '
        }
        entryData


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


squareAbove : Grid -> Coordinate -> Maybe Square
squareAbove grid coordinate =
    squareAtCoordinate grid <| Coordinate.above coordinate


squareAtLeft : Grid -> Coordinate -> Maybe Square
squareAtLeft grid coordinate =
    squareAtCoordinate grid <| Coordinate.atLeft coordinate


hasLetterSquareAt : Grid -> Coordinate -> Bool
hasLetterSquareAt grid coordinate =
    squareAtCoordinate grid coordinate
        |> Maybe.map Square.isLetterSquare
        |> Maybe.withDefault False


hasLetterSquareAbove : Grid -> Coordinate -> Bool
hasLetterSquareAbove grid coordinate =
    squareAbove grid coordinate
        |> Maybe.map Square.isLetterSquare
        |> Maybe.withDefault False


hasLetterSquareAtLeft : Grid -> Coordinate -> Bool
hasLetterSquareAtLeft grid coordinate =
    squareAtLeft grid coordinate
        |> Maybe.map Square.isLetterSquare
        |> Maybe.withDefault False


setGuess : Coordinate -> Char -> Grid -> Grid
setGuess ( x, y ) newGuess grid =
    let
        updateGuess =
            (\square ->
                case square of
                    LetterSquare coord letterData entryData ->
                        LetterSquare coord { letterData | guess = newGuess } entryData

                    BlockSquare coord ->
                        BlockSquare coord
            )
    in
        Matrix.update x y updateGuess grid


acrossClues : Grid -> List String
acrossClues grid =
    let
        maybesToList maybes =
            List.map
                (\m ->
                    case m of
                        Just x ->
                            [ x ]

                        Nothing ->
                            []
                )
                maybes
                |> List.concat
    in
        Matrix.map Square.acrossClue grid
            |> Matrix.toIndexedArray
            |> Array.toList
            |> List.map (\( ( i, j ), c ) -> c)
            |> maybesToList


downClues : Grid -> List String
downClues grid =
    let
        maybesToList maybes =
            List.map
                (\m ->
                    case m of
                        Just x ->
                            [ x ]

                        Nothing ->
                            []
                )
                maybes
                |> List.concat
    in
        Matrix.map Square.downClue grid
            |> Matrix.toIndexedArray
            |> Array.toList
            |> List.map (\( ( i, j ), c ) -> c)
            |> maybesToList
