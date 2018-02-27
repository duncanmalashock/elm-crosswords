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
                            [ LetterSquare ( 0, 0 ) ' '
                            , BlockSquare ( 1, 0 )
                            , BlockSquare ( 0, 1 )
                            , LetterSquare ( 1, 1 ) ' '
                            ]

                        input =
                            ".**."
                    in
                        Expect.equal (Grid.fromString 2 2 input) expectedOutput
            , skip <|
                test "decodes a string into a Grid" <|
                    \_ ->
                        let
                            expectedOutput =
                                [ LetterSquare ( 1, 0 ) ' '
                                , BlockSquare ( 2, 0 )
                                , LetterSquare ( 0, 1 ) ' '
                                , LetterSquare ( 1, 1 ) ' '
                                , LetterSquare ( 2, 1 ) ' '
                                , BlockSquare ( 0, 2 )
                                , LetterSquare ( 1, 2 ) ' '
                                , LetterSquare ( 2, 2 ) ' '
                                ]

                            input =
                                "3,3[..*...*..]"
                        in
                            Expect.equal (Grid.fromString 3 3 input) expectedOutput
            ]
        ]
