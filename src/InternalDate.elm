module InternalDate exposing (parse, toDate)

import Date exposing (Date, Day(..), Month(..))
import Date.Extra.Config
import Date.Extra.Create
import Error exposing (Error(..))
import InternalDate.Parser exposing (fromPattern)
import InternalDate.Type exposing (AmPm(..), InternalDate, emptyDate)
import Parser exposing ((|.), (|=), Parser, inContext, succeed)
import Pattern exposing (Pattern(..))
import Utilities exposing (monthFromInt, monthToInt, toUpper)


parse : String -> Date.Extra.Config.Config -> List Pattern -> Result Error InternalDate
parse str config patterns =
    let
        parser =
            patterns
                |> List.map (fromPattern config)
                |> List.foldl (\a b -> b |> Parser.andThen a) (succeed emptyDate)
    in
    Parser.run parser str
        |> Result.mapError ParsingError


toDate : InternalDate -> Result Error Date
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
        |> Result.mapError DateError


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
                Err "Invalid hour"


toHour : AmPm -> Int -> Int
toHour ampm hour =
    case ( ampm, hour ) of
        ( AM, 12 ) ->
            0

        ( AM, _ ) ->
            hour

        ( PM, 12 ) ->
            12

        ( PM, _ ) ->
            hour + 12
