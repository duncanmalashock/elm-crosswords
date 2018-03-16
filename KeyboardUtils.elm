module KeyboardUtils exposing (..)

import Keyboard.Extra exposing (Key(..))


letterKeys =
    [ Keyboard.Extra.CharA
    , Keyboard.Extra.CharB
    , Keyboard.Extra.CharC
    , Keyboard.Extra.CharD
    , Keyboard.Extra.CharE
    , Keyboard.Extra.CharF
    , Keyboard.Extra.CharG
    , Keyboard.Extra.CharH
    , Keyboard.Extra.CharI
    , Keyboard.Extra.CharJ
    , Keyboard.Extra.CharK
    , Keyboard.Extra.CharL
    , Keyboard.Extra.CharM
    , Keyboard.Extra.CharN
    , Keyboard.Extra.CharO
    , Keyboard.Extra.CharP
    , Keyboard.Extra.CharQ
    , Keyboard.Extra.CharR
    , Keyboard.Extra.CharS
    , Keyboard.Extra.CharT
    , Keyboard.Extra.CharU
    , Keyboard.Extra.CharV
    , Keyboard.Extra.CharW
    , Keyboard.Extra.CharX
    , Keyboard.Extra.CharY
    , Keyboard.Extra.CharZ
    ]


containsLetterKeys keyList =
    List.any (\k -> List.member k letterKeys) keyList


filterOnlyLetterKeys : List Key -> List Key
filterOnlyLetterKeys keyList =
    List.filter (\k -> List.member k letterKeys) keyList


toLetterChar : Key -> Char
toLetterChar k =
    case k of
        Keyboard.Extra.CharA ->
            'A'

        Keyboard.Extra.CharB ->
            'B'

        Keyboard.Extra.CharC ->
            'C'

        Keyboard.Extra.CharD ->
            'D'

        Keyboard.Extra.CharE ->
            'E'

        Keyboard.Extra.CharF ->
            'F'

        Keyboard.Extra.CharG ->
            'G'

        Keyboard.Extra.CharH ->
            'H'

        Keyboard.Extra.CharI ->
            'I'

        Keyboard.Extra.CharJ ->
            'J'

        Keyboard.Extra.CharK ->
            'K'

        Keyboard.Extra.CharL ->
            'L'

        Keyboard.Extra.CharM ->
            'M'

        Keyboard.Extra.CharN ->
            'N'

        Keyboard.Extra.CharO ->
            'O'

        Keyboard.Extra.CharP ->
            'P'

        Keyboard.Extra.CharQ ->
            'Q'

        Keyboard.Extra.CharR ->
            'R'

        Keyboard.Extra.CharS ->
            'S'

        Keyboard.Extra.CharT ->
            'T'

        Keyboard.Extra.CharU ->
            'U'

        Keyboard.Extra.CharV ->
            'V'

        Keyboard.Extra.CharW ->
            'W'

        Keyboard.Extra.CharX ->
            'X'

        Keyboard.Extra.CharY ->
            'Y'

        Keyboard.Extra.CharZ ->
            'Z'

        _ ->
            ' '
