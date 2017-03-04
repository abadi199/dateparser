module DateParserTests exposing (tests)

import Test exposing (..)
import Expect exposing (Expectation)
import DateParser
import Date exposing (Month(..))
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Create
import TestHelpers exposing (mapOk, mapError, join)


tests : Test
tests =
    describe "DateParser.parse tests"
        [ test "DateParser.parse 12/31/2017" <|
            \() ->
                DateParser.parse config "%m/%d/%Y" "12/31/2017"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 2017 Dec 31 0 0 0 0)
        , test "DateParser.parse %d/%m/%Y" <|
            \() ->
                DateParser.parse config "%d/%m/%Y" "31/12/2017"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 2017 Dec 31 0 0 0 0)
        , test "DateParser.parse July 15th, 1998" <|
            \() ->
                DateParser.parse config "%B %-@d, %Y" "July 15th, 1998"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 1998 Jul 15 0 0 0 0)
        , test "DateParser.parse 11:59 PM" <|
            \() ->
                DateParser.parse config "%I:%M %p" "11:59 PM"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 1900 Jan 1 23 59 0 0)
        , test "DateParser.parse 09:01 AM" <|
            \() ->
                DateParser.parse config "%I:%M %p" "09:01 AM"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 1900 Jan 1 9 1 0 0)
        , test "DateParser.parse 11:59" <|
            \() ->
                DateParser.parse config "%H:%M" "11:59"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 1900 Jan 1 11 59 0 0)
        , test "DateParser.parse 21:01" <|
            \() ->
                DateParser.parse config "%H:%M" "21:01"
                    |> Expect.equal (Ok <| Date.Extra.Create.dateFromFields 1900 Jan 1 21 1 0 0)
        ]
