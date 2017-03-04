module InternalDate exposing (parse, toDate)

import Pattern exposing (Pattern(..))
import Parser exposing (Error, Parser, succeed, (|=), (|.), inContext)
import String
import Date.Extra.Config
import Date exposing (Month(..), Day(..), Date)
import Utilities exposing (toUpper, monthToInt, monthFromInt)
import InternalDate.Type exposing (InternalDate, emptyDate, AmPm(..))
import InternalDate.Parser exposing (fromPattern)
import Date.Extra.Create


parse : String -> Date.Extra.Config.Config -> List Pattern -> Result String InternalDate
parse str config patterns =
    let
        parser =
            patterns
                |> List.map (fromPattern config)
                |> List.foldl (\a b -> b |> Parser.andThen a) (succeed emptyDate)
    in
        Parser.run parser str
            |> Result.mapError (always "InternalDate.parse error")


toDate : InternalDate -> Result String Date
toDate internalDate =
    let
        monthResult =
            monthFromInt internalDate.month

        hourResult =
            toHour24 internalDate
    in
        Result.map2
            (\month hour ->
                Date.Extra.Create.dateFromFields
                    internalDate.year
                    month
                    internalDate.date
                    hour
                    internalDate.minute
                    internalDate.second
                    internalDate.millisecond
            )
            monthResult
            hourResult


toHour24 : InternalDate -> Result String Int
toHour24 internalDate =
    let
        hour =
            internalDate.hour
    in
        case internalDate.ampm of
            Nothing ->
                Ok hour

            Just ampm ->
                if hour > 0 && hour <= 12 then
                    Ok (toHour ampm hour)
                else
                    (Err "Invalid hour")


toHour : AmPm -> Int -> Int
toHour ampm hour =
    case ( ampm, hour ) of
        ( AM, 12 ) ->
            0

        ( AM, _ ) ->
            hour

        ( PM, _ ) ->
            hour + 12
