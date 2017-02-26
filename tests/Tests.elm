module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import DateParser
import Date


all : Test
all =
    describe "Date Parser Test Suite"
        [ describe "DateParser.parse tests"
            [ test "DateParser.parse %d/%m/%Y" <|
                \() ->
                    DateParser.parse "%d/%m/%Y" "2017-12-31"
                        |> Expect.equal (Date.fromString "2017-12-31")
            ]
        ]
