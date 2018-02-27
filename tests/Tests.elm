module Tests exposing (..)

import Grid exposing (Square(..))
import Test exposing (Test, describe, test, skip)
import Expect


suite : Test
suite =
    describe "Grid"
        [ describe "fromString"
            [ test "one-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ LetterSquare ( 0, 0 ) ' '
                                ]

                        input =
                            "."
                    in
                        Expect.equal (Grid.fromString 2 2 input) expectedOutput
            , test "two-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ LetterSquare ( 0, 0 ) ' '
                                , LetterSquare ( 1, 0 ) ' '
                                ]

                        input =
                            ".."
                    in
                        Expect.equal (Grid.fromString 2 2 input) expectedOutput
            , test "four-squared Grid" <|
                \_ ->
                    let
                        expectedOutput =
                            Ok
                                [ LetterSquare ( 0, 0 ) ' '
                                , BlockSquare ( 1, 0 )
                                , BlockSquare ( 0, 1 )
                                , LetterSquare ( 1, 1 ) ' '
                                ]

                        input =
                            ".**."
                    in
                        Expect.equal (Grid.fromString 2 2 input) expectedOutput
            , test "invalid characters" <|
                \_ ->
                    let
                        expectedOutput =
                            Err "Invalid characters"

                        input =
                            "junk"
                    in
                        Expect.equal (Grid.fromString 2 2 input) expectedOutput
            ]
        ]
