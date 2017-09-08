module PatternTests exposing (otherTests, parseTests)

import Expect exposing (Expectation)
import Parser
import Pattern exposing (Pattern(..))
import Test exposing (..)
import TestHelpers exposing (join, mapError, mapOk)


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
        [ test "(empty pattern)" <|
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
