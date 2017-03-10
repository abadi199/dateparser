module PatternTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import Pattern exposing (Pattern(..))
import TestHelpers exposing (mapOk, mapError, join)
import Parser
import Error exposing (Error(..))


tests : Test
tests =
    describe "Pattern Test Suite"
        [ parseTests
        , otherTests
        ]


parseTests : Test
parseTests =
    describe "Pattern.parse"
        [ test "Pattern.parse %Y" <|
            \() ->
                Pattern.parse "%Y"
                    |> mapOk (Expect.equal [ Year ])
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Pattern.parse %d/%m/%Y" <|
            \() ->
                Pattern.parse "%d/%m/%Y"
                    |> mapOk (Expect.equal [ DateZeroPadded, Other "/", MonthZeroPadded, Other "/", Year ])
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Pattern.parse %d - %m - %Y" <|
            \() ->
                Pattern.parse "%d - %m - %Y"
                    |> mapOk (Expect.equal [ DateZeroPadded, Other " - ", MonthZeroPadded, Other " - ", Year ])
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]


otherTests : Test
otherTests =
    describe "Pattern.other"
        [ test "" <|
            \() ->
                Parser.run Pattern.other ""
                    |> mapOk (always <| Expect.fail "should fail")
                    |> mapError (always Expect.pass)
                    |> join
        , test "//%Y" <|
            \() ->
                Parser.run Pattern.other "//%Y"
                    |> mapOk (Expect.equal (Other "//"))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "//" <|
            \() ->
                Parser.run Pattern.other "//"
                    |> mapOk (Expect.equal (Other "//"))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "-" <|
            \() ->
                Parser.run Pattern.other "-"
                    |> mapOk (Expect.equal (Other "-"))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "-%m" <|
            \() ->
                Parser.run Pattern.other "-"
                    |> mapOk (Expect.equal (Other "-"))
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]
