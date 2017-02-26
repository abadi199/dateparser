module PatternTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import Pattern exposing (Pattern(..))
import TestHelpers exposing (mapOk, mapError, join)
import Parser


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
        , test "Pattern.parse %d%m%e" <|
            \() ->
                Pattern.parse "%d%m%e"
                    |> mapOk (always <| Expect.fail "should fail")
                    |> mapError (Expect.equal "Invalid pattern: duplicate date")
                    |> join
        , test "Pattern.parse %m/%m" <|
            \() ->
                Pattern.parse "%m/%m"
                    |> mapOk (always <| Expect.fail "should fail")
                    |> mapError (Expect.equal "Invalid pattern: duplicate month")
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
