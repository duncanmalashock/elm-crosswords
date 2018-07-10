module Square exposing (..)

import Coordinate exposing (Coordinate)
import Direction exposing (Direction(..))
import Char


type Square
    = LetterSquare Coordinate LetterData EntryData
    | BlockSquare Coordinate


type alias LetterData =
    { solution : Char
    , guess : Char
    }


type alias EntryData =
    { startsEntries : StartsEntries
    , inAcrossEntry : Int
    , inDownEntry : Int
    }


type alias ClueData =
    { clue : String
    }


type StartsEntries
    = StartsAcross ClueData
    | StartsDown ClueData
    | StartsAcrossAndDown ClueData ClueData
    | NoStart


blockChar : Char
blockChar =
    '*'


blankChar : Char
blankChar =
    '.'


letterChars : List Char
letterChars =
    String.toList ".ABCDEFGHIJKLMNOPQRSTUVWXYZ"


isLetterChar : Char -> Bool
isLetterChar char =
    List.member (Char.toUpper char) letterChars


isLetterSquare : Square -> Bool
isLetterSquare square =
    case square of
        LetterSquare _ _ _ ->
            True

        BlockSquare _ ->
            False


entryNumber : Square -> Maybe Int
entryNumber square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross _ ->
                    Just entryData.inAcrossEntry

                StartsDown _ ->
                    Just entryData.inDownEntry

                StartsAcrossAndDown _ _ ->
                    Just entryData.inAcrossEntry

                NoStart ->
                    Nothing

        BlockSquare _ ->
            Nothing


isInEntry : Int -> Direction -> Square -> Bool
isInEntry entryNumber direction square =
    case square of
        LetterSquare _ _ entryData ->
            case direction of
                Across ->
                    entryNumber == entryData.inAcrossEntry

                Down ->
                    entryNumber == entryData.inDownEntry

        BlockSquare _ ->
            False


startsEntry : Int -> Direction -> Square -> Bool
startsEntry entryNumber direction square =
    case square of
        LetterSquare _ _ entryData ->
            case direction of
                Across ->
                    (entryNumber == entryData.inAcrossEntry)
                        && startsAcrossEntry square

                Down ->
                    (entryNumber == entryData.inDownEntry)
                        && startsDownEntry square

        BlockSquare _ ->
            False


startsAcrossEntry : Square -> Bool
startsAcrossEntry square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross _ ->
                    True

                StartsDown _ ->
                    False

                StartsAcrossAndDown _ _ ->
                    True

                NoStart ->
                    False

        BlockSquare _ ->
            False


startsDownEntry : Square -> Bool
startsDownEntry square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross _ ->
                    False

                StartsDown _ ->
                    True

                StartsAcrossAndDown _ _ ->
                    True

                NoStart ->
                    False

        BlockSquare _ ->
            False


acrossClue : Square -> Maybe ( Int, String )
acrossClue square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross clueData ->
                    Just ( entryData.inAcrossEntry, clueData.clue )

                StartsDown _ ->
                    Nothing

                StartsAcrossAndDown clueData _ ->
                    Just ( entryData.inAcrossEntry, clueData.clue )

                NoStart ->
                    Nothing

        BlockSquare _ ->
            Nothing


downClue : Square -> Maybe ( Int, String )
downClue square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross _ ->
                    Nothing

                StartsDown clueData ->
                    Just ( entryData.inDownEntry, clueData.clue )

                StartsAcrossAndDown _ clueData ->
                    Just ( entryData.inDownEntry, clueData.clue )

                NoStart ->
                    Nothing

        BlockSquare _ ->
            Nothing


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


isGuessed : Square -> Bool
isGuessed square =
    case square of
        LetterSquare _ letterData _ ->
            letterData.guess /= ' '

        BlockSquare _ ->
            True


hasCorrectGuess : Square -> Bool
hasCorrectGuess square =
    case square of
        LetterSquare _ letterData _ ->
            letterData.guess == letterData.solution

        BlockSquare _ ->
            True


toString : Square -> String
toString s =
    case s of
        LetterSquare _ letterData _ ->
            String.fromChar letterData.solution

        BlockSquare _ ->
            "*"
