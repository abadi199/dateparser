module Utilities
    exposing
        ( monthFromInt
        , monthToInt
        , toUpper
        )

import Date exposing (Month(..))


toUpper : Bool -> String -> String
toUpper upper =
    if upper then
        String.toUpper
    else
        identity


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


monthFromInt : Int -> Result String Date.Month
monthFromInt month =
    case month of
        1 ->
            Ok Jan

        2 ->
            Ok Feb

        3 ->
            Ok Mar

        4 ->
            Ok Apr

        5 ->
            Ok May

        6 ->
            Ok Jun

        7 ->
            Ok Jul

        8 ->
            Ok Aug

        9 ->
            Ok Sep

        10 ->
            Ok Oct

        11 ->
            Ok Nov

        12 ->
            Ok Dec

        _ ->
            Err ("Invalid month: " ++ toString month)
