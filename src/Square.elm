module Square exposing (..)

import Coordinate exposing (Coordinate)
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


acrossClue : Square -> Maybe String
acrossClue square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross clueData ->
                    Just clueData.clue

                StartsDown _ ->
                    Nothing

                StartsAcrossAndDown clueDataAcross _ ->
                    Just clueDataAcross.clue

                NoStart ->
                    Nothing

        BlockSquare _ ->
            Nothing


downClue : Square -> Maybe String
downClue square =
    case square of
        LetterSquare _ _ entryData ->
            case entryData.startsEntries of
                StartsAcross _ ->
                    Nothing

                StartsDown clueData ->
                    Just clueData.clue

                StartsAcrossAndDown _ clueDataDown ->
                    Just clueDataDown.clue

                NoStart ->
                    Nothing

        BlockSquare _ ->
            Nothing


toString : Square -> String
toString s =
    case s of
        LetterSquare _ letterData _ ->
            String.fromChar letterData.solution

        BlockSquare _ ->
            "*"
