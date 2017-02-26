module DateParserTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import DateParser
import Date


tests : Test
tests =
    describe "DateParser.parse tests"
        [ test "DateParser.parse %d/%m/%Y" <|
            \() ->
                DateParser.parse "%d/%m/%Y" "2017-12-31"
                    |> Expect.equal (Date.fromString "2017-12-31")
        ]
