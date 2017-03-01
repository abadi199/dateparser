module InternalDateTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import InternalDate exposing (InternalDate, parse, emptyDate, AmPm(..))
import TestHelpers exposing (mapOk, mapError, join)
import Parser
import Pattern exposing (Pattern(..))


tests : Test
tests =
    describe "InternalDate Test Suite"
        [ parseTests
        ]


parseTests : Test
parseTests =
    describe "InternalDate.parse"
        [ test "InternalDate.parse \"2017\"" <|
            \() ->
                InternalDate.parse [ Year ] "2017"
                    |> mapOk (.year >> Expect.equal 2017)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"12/2017\"" <|
            \() ->
                InternalDate.parse [ MonthZeroPadded, Other "/", Year ] "12/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01/2017\"" <|
            \() ->
                InternalDate.parse [ MonthZeroPadded, Other "/", Year ] "01/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"1/2017\"" <|
            \() ->
                InternalDate.parse [ MonthZeroPadded, Other "/", Year ] "1/2017"
                    |> mapOk (toString >> Expect.fail)
                    |> mapError (always Expect.pass)
                    |> join
        , test "InternalDate.parse \"31/12/2017\"" <|
            \() ->
                InternalDate.parse [ DateZeroPadded, Other "/", MonthZeroPadded, Other "/", Year ] "31/12/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"31 December 2017\"" <|
            \() ->
                InternalDate.parse [ DateZeroPadded, Other " ", MonthFullName, Other " ", Year ] "31 December 2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01 Apr 2017\"" <|
            \() ->
                InternalDate.parse [ DateZeroPadded, Other " ", MonthAbbrvName, Other " ", Year ] "01 Apr 2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 4, date = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"12/ 5/1998\"" <|
            \() ->
                InternalDate.parse [ MonthZeroPadded, Other "/", DateSpacePadded, Other "/", Year ] "12/ 5/1998"
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 12, date = 5 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"05/01/1998 23:15:21\"" <|
            \() ->
                InternalDate.parse
                    [ MonthZeroPadded
                    , Other "/"
                    , DateZeroPadded
                    , Other "/"
                    , Year
                    , Other " "
                    , Hour24ZeroPadded
                    , Other ":"
                    , MinuteZeroPadded
                    , Other ":"
                    , SecondZeroPadded
                    ]
                    "05/01/1998 23:15:21"
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 5, date = 1, hour = 23, minute = 15, second = 21 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"05/01/1998 10:15:21 PM\"" <|
            \() ->
                InternalDate.parse
                    [ MonthZeroPadded
                    , Other "/"
                    , DateZeroPadded
                    , Other "/"
                    , Year
                    , Other " "
                    , Hour24ZeroPadded
                    , Other ":"
                    , MinuteZeroPadded
                    , Other ":"
                    , SecondZeroPadded
                    , Other " "
                    , AMPM
                    ]
                    "05/01/1998 10:15:21 PM"
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 5, date = 1, hour = 10, minute = 15, second = 21, ampm = Just PM })
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]
