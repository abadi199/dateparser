module InternalDateTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import InternalDate exposing (InternalDate, parse, emptyDate, AmPm(..))
import TestHelpers exposing (mapOk, mapError, join)
import Parser
import Pattern exposing (Pattern(..))
import Date.Extra.Config.Config_en_us exposing (config)


tests : Test
tests =
    describe "InternalDate Test Suite"
        [ parseTests
        , individualParserTests
        ]


individualParserTests : Test
individualParserTests =
    describe "Individual InternalDate.parse"
        [ test "Year" <|
            \() ->
                InternalDate.parse config [ Year ] "2017"
                    |> mapOk (.year >> Expect.equal 2017)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthZeroPadded" <|
            \() ->
                InternalDate.parse config [ MonthZeroPadded ] "03"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthSpacePadded" <|
            \() ->
                InternalDate.parse config [ MonthSpacePadded ] " 3"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Month" <|
            \() ->
                InternalDate.parse config [ Month ] "3"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthFullName" <|
            \() ->
                InternalDate.parse config [ MonthFullName ] "March"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthUpperFullName" <|
            \() ->
                InternalDate.parse config [ MonthUpperFullName ] "MARCH"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthAbbrvName" <|
            \() ->
                InternalDate.parse config [ MonthAbbrvName ] "Mar"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthUpperAbbrvName" <|
            \() ->
                InternalDate.parse config [ MonthUpperAbbrvName ] "MAR"
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateZeroPadded" <|
            \() ->
                InternalDate.parse config [ DateZeroPadded ] "07"
                    |> mapOk (.date >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSpacePadded" <|
            \() ->
                InternalDate.parse config [ DateSpacePadded ] " 7"
                    |> mapOk (.date >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Date" <|
            \() ->
                InternalDate.parse config [ Date ] "15"
                    |> mapOk (.date >> Expect.equal 15)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSuffix" <|
            \() ->
                InternalDate.parse config [ DateSuffix ] "3rd"
                    |> mapOk (.date >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSpacePaddedSuffix" <|
            \() ->
                InternalDate.parse config [ DateSpacePaddedSuffix ] " 3rd"
                    |> mapOk (.date >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekFullName" <|
            \() ->
                InternalDate.parse config [ DayOfWeekFullName ] "Monday"
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekUpperFullName" <|
            \() ->
                InternalDate.parse config [ DayOfWeekUpperFullName ] "TUESDAY"
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekAbbrvName" <|
            \() ->
                InternalDate.parse config [ DayOfWeekAbbrvName ] "Wed"
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekUpperAbbrvName" <|
            \() ->
                InternalDate.parse config [ DayOfWeekUpperAbbrvName ] "THU"
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24ZeroPadded" <|
            \() ->
                InternalDate.parse config [ Hour24ZeroPadded ] "03"
                    |> mapOk (.hour >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24SpacePadded" <|
            \() ->
                InternalDate.parse config [ Hour24SpacePadded ] " 4"
                    |> mapOk (.hour >> Expect.equal 4)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24" <|
            \() ->
                InternalDate.parse config [ Hour24 ] "23"
                    |> mapOk (.hour >> Expect.equal 23)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12ZeroPadded" <|
            \() ->
                InternalDate.parse config [ Hour12ZeroPadded ] "07"
                    |> mapOk (.hour >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12SpacePadded" <|
            \() ->
                InternalDate.parse config [ Hour12SpacePadded ] " 9"
                    |> mapOk (.hour >> Expect.equal 9)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12" <|
            \() ->
                InternalDate.parse config [ Hour12 ] "12"
                    |> mapOk (.hour >> Expect.equal 12)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "AMPM" <|
            \() ->
                InternalDate.parse config [ AMPM ] "PM"
                    |> mapOk (.ampm >> Expect.equal (Just PM))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Ampm" <|
            \() ->
                InternalDate.parse config [ Ampm ] "am"
                    |> mapOk (.ampm >> Expect.equal (Just AM))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MinuteZeroPadded" <|
            \() ->
                InternalDate.parse config [ MinuteZeroPadded ] "59"
                    |> mapOk (.minute >> Expect.equal 59)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "SecondZeroPadded" <|
            \() ->
                InternalDate.parse config [ SecondZeroPadded ] "00"
                    |> mapOk (.second >> Expect.equal 0)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse config [ Millisecond ] "000"
                    |> mapOk (.millisecond >> Expect.equal 0)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse config [ Millisecond ] "008"
                    |> mapOk (.millisecond >> Expect.equal 8)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse config [ Millisecond ] "023"
                    |> mapOk (.millisecond >> Expect.equal 23)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse config [ Millisecond ] "999"
                    |> mapOk (.millisecond >> Expect.equal 999)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse config [ Millisecond ] "9998"
                    |> mapOk (always (Expect.fail "should fail"))
                    |> mapError (always Expect.pass)
                    |> join
        , test "TimeZoneOffsetColon" <|
            \() ->
                InternalDate.parse config [ TimeZoneOffsetColon ] "+10:30"
                    |> mapOk (.timeZoneOffset >> Expect.equal -630)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffsetColon" <|
            \() ->
                InternalDate.parse config [ TimeZoneOffsetColon ] "-06:30"
                    |> mapOk (.timeZoneOffset >> Expect.equal 390)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffset" <|
            \() ->
                InternalDate.parse config [ TimeZoneOffset ] "+1030"
                    |> mapOk (.timeZoneOffset >> Expect.equal -630)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffset" <|
            \() ->
                InternalDate.parse config [ TimeZoneOffset ] "-0630"
                    |> mapOk (.timeZoneOffset >> Expect.equal 390)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Other" <|
            \() ->
                InternalDate.parse config [ Other "/" ] "/"
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]


parseTests : Test
parseTests =
    describe "InternalDate.parse"
        [ test "InternalDate.parse \"12/2017\"" <|
            \() ->
                InternalDate.parse config [ MonthZeroPadded, Other "/", Year ] "12/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01/2017\"" <|
            \() ->
                InternalDate.parse config [ MonthZeroPadded, Other "/", Year ] "01/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"1/2017\"" <|
            \() ->
                InternalDate.parse config [ MonthZeroPadded, Other "/", Year ] "1/2017"
                    |> mapOk (toString >> Expect.fail)
                    |> mapError (always Expect.pass)
                    |> join
        , test "InternalDate.parse \"31/12/2017\"" <|
            \() ->
                InternalDate.parse config [ DateZeroPadded, Other "/", MonthZeroPadded, Other "/", Year ] "31/12/2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"31 December 2017\"" <|
            \() ->
                InternalDate.parse config [ DateZeroPadded, Other " ", MonthFullName, Other " ", Year ] "31 December 2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01 Apr 2017\"" <|
            \() ->
                InternalDate.parse config [ DateZeroPadded, Other " ", MonthAbbrvName, Other " ", Year ] "01 Apr 2017"
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 4, date = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"12/ 5/1998\"" <|
            \() ->
                InternalDate.parse config [ MonthZeroPadded, Other "/", DateSpacePadded, Other "/", Year ] "12/ 5/1998"
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 12, date = 5 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"05/01/1998 23:15:21\"" <|
            \() ->
                InternalDate.parse config
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
                InternalDate.parse config
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
