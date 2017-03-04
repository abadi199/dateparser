module InternalDateTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import InternalDate exposing (parse)
import InternalDate.Type exposing (InternalDate, emptyDate, AmPm(..))
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
                InternalDate.parse "2017" config [ Year ]
                    |> mapOk (.year >> Expect.equal 2017)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthZeroPadded" <|
            \() ->
                InternalDate.parse "03" config [ MonthZeroPadded ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthSpacePadded" <|
            \() ->
                InternalDate.parse " 3" config [ MonthSpacePadded ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Month" <|
            \() ->
                InternalDate.parse "3" config [ Month ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthFullName" <|
            \() ->
                InternalDate.parse "March" config [ MonthFullName ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthUpperFullName" <|
            \() ->
                InternalDate.parse "MARCH" config [ MonthUpperFullName ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthAbbrvName" <|
            \() ->
                InternalDate.parse "Mar" config [ MonthAbbrvName ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MonthUpperAbbrvName" <|
            \() ->
                InternalDate.parse "MAR" config [ MonthUpperAbbrvName ]
                    |> mapOk (.month >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateZeroPadded" <|
            \() ->
                InternalDate.parse "07" config [ DateZeroPadded ]
                    |> mapOk (.date >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSpacePadded" <|
            \() ->
                InternalDate.parse " 7" config [ DateSpacePadded ]
                    |> mapOk (.date >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Date" <|
            \() ->
                InternalDate.parse "15" config [ Date ]
                    |> mapOk (.date >> Expect.equal 15)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSuffix" <|
            \() ->
                InternalDate.parse "3rd" config [ DateSuffix ]
                    |> mapOk (.date >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DateSpacePaddedSuffix" <|
            \() ->
                InternalDate.parse " 3rd" config [ DateSpacePaddedSuffix ]
                    |> mapOk (.date >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekFullName" <|
            \() ->
                InternalDate.parse "Monday" config [ DayOfWeekFullName ]
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekUpperFullName" <|
            \() ->
                InternalDate.parse "TUESDAY" config [ DayOfWeekUpperFullName ]
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekAbbrvName" <|
            \() ->
                InternalDate.parse "Wed" config [ DayOfWeekAbbrvName ]
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "DayOfWeekUpperAbbrvName" <|
            \() ->
                InternalDate.parse "THU" config [ DayOfWeekUpperAbbrvName ]
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24ZeroPadded" <|
            \() ->
                InternalDate.parse "03" config [ Hour24ZeroPadded ]
                    |> mapOk (.hour >> Expect.equal 3)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24SpacePadded" <|
            \() ->
                InternalDate.parse " 4" config [ Hour24SpacePadded ]
                    |> mapOk (.hour >> Expect.equal 4)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour24" <|
            \() ->
                InternalDate.parse "23" config [ Hour24 ]
                    |> mapOk (.hour >> Expect.equal 23)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12ZeroPadded" <|
            \() ->
                InternalDate.parse "07" config [ Hour12ZeroPadded ]
                    |> mapOk (.hour >> Expect.equal 7)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12SpacePadded" <|
            \() ->
                InternalDate.parse " 9" config [ Hour12SpacePadded ]
                    |> mapOk (.hour >> Expect.equal 9)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Hour12" <|
            \() ->
                InternalDate.parse "12" config [ Hour12 ]
                    |> mapOk (.hour >> Expect.equal 12)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "AMPM" <|
            \() ->
                InternalDate.parse "PM" config [ AMPM ]
                    |> mapOk (.ampm >> Expect.equal (Just PM))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Ampm" <|
            \() ->
                InternalDate.parse "am" config [ Ampm ]
                    |> mapOk (.ampm >> Expect.equal (Just AM))
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "MinuteZeroPadded" <|
            \() ->
                InternalDate.parse "59" config [ MinuteZeroPadded ]
                    |> mapOk (.minute >> Expect.equal 59)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "SecondZeroPadded" <|
            \() ->
                InternalDate.parse "00" config [ SecondZeroPadded ]
                    |> mapOk (.second >> Expect.equal 0)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse "000" config [ Millisecond ]
                    |> mapOk (.millisecond >> Expect.equal 0)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse "008" config [ Millisecond ]
                    |> mapOk (.millisecond >> Expect.equal 8)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse "023" config [ Millisecond ]
                    |> mapOk (.millisecond >> Expect.equal 23)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse "999" config [ Millisecond ]
                    |> mapOk (.millisecond >> Expect.equal 999)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Millisecond" <|
            \() ->
                InternalDate.parse "9998" config [ Millisecond ]
                    |> mapOk (always (Expect.fail "should fail"))
                    |> mapError (always Expect.pass)
                    |> join
        , test "TimeZoneOffsetColon" <|
            \() ->
                InternalDate.parse "+10:30" config [ TimeZoneOffsetColon ]
                    |> mapOk (.timeZoneOffset >> Expect.equal -630)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffsetColon" <|
            \() ->
                InternalDate.parse "-06:30" config [ TimeZoneOffsetColon ]
                    |> mapOk (.timeZoneOffset >> Expect.equal 390)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffset" <|
            \() ->
                InternalDate.parse "+1030" config [ TimeZoneOffset ]
                    |> mapOk (.timeZoneOffset >> Expect.equal -630)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "TimeZoneOffset" <|
            \() ->
                InternalDate.parse "-0630" config [ TimeZoneOffset ]
                    |> mapOk (.timeZoneOffset >> Expect.equal 390)
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "Other" <|
            \() ->
                InternalDate.parse "/" config [ Other "/" ]
                    |> mapOk (always Expect.pass)
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]


parseTests : Test
parseTests =
    describe "InternalDate.parse"
        [ test "InternalDate.parse \"12/2017\"" <|
            \() ->
                InternalDate.parse "12/2017" config [ MonthZeroPadded, Other "/", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01/2017\"" <|
            \() ->
                InternalDate.parse "01/2017" config [ MonthZeroPadded, Other "/", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"1/2017\"" <|
            \() ->
                InternalDate.parse "1/2017" config [ MonthZeroPadded, Other "/", Year ]
                    |> mapOk (toString >> Expect.fail)
                    |> mapError (always Expect.pass)
                    |> join
        , test "InternalDate.parse \"31/12/2017\"" <|
            \() ->
                InternalDate.parse "31/12/2017" config [ DateZeroPadded, Other "/", MonthZeroPadded, Other "/", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"31 December 2017\"" <|
            \() ->
                InternalDate.parse "31 December 2017" config [ DateZeroPadded, Other " ", MonthFullName, Other " ", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 12, date = 31 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"01 Apr 2017\"" <|
            \() ->
                InternalDate.parse "01 Apr 2017" config [ DateZeroPadded, Other " ", MonthAbbrvName, Other " ", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 4, date = 1 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"12/ 5/1998\"" <|
            \() ->
                InternalDate.parse "12/ 5/1998" config [ MonthZeroPadded, Other "/", DateSpacePadded, Other "/", Year ]
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 12, date = 5 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"05/01/1998 23:15:21\"" <|
            \() ->
                InternalDate.parse "05/01/1998 23:15:21"
                    config
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
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 5, date = 1, hour = 23, minute = 15, second = 21 })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"05/01/1998 10:15:21 PM\"" <|
            \() ->
                InternalDate.parse "05/01/1998 10:15:21 PM"
                    config
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
                    |> mapOk (Expect.equal { emptyDate | year = 1998, month = 5, date = 1, hour = 10, minute = 15, second = 21, ampm = Just PM })
                    |> mapError (toString >> Expect.fail)
                    |> join
        , test "InternalDate.parse \"Fri, Mar 3rd, 2017, 10:15:21 PM\"" <|
            \() ->
                InternalDate.parse "Fri, Mar 3rd, 2017, 10:15:21 PM"
                    config
                    [ DayOfWeekAbbrvName
                    , Other ", "
                    , MonthAbbrvName
                    , Other " "
                    , DateSuffix
                    , Other ", "
                    , Year
                    , Other ", "
                    , Hour24ZeroPadded
                    , Other ":"
                    , MinuteZeroPadded
                    , Other ":"
                    , SecondZeroPadded
                    , Other " "
                    , AMPM
                    ]
                    |> mapOk (Expect.equal { emptyDate | year = 2017, month = 3, date = 3, hour = 10, minute = 15, second = 21, ampm = Just PM })
                    |> mapError (toString >> Expect.fail)
                    |> join
        ]
