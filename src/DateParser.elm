module DateParser exposing (parse)

{-|
Custom String to Date parser.
This library uses date format from https://github.com/rluiten/elm-date-extra/blob/8.2.0/DocFormat.md

# Parsing
@docs parse

-}

import Pattern
import Date exposing (Date)
import InternalDate
import Date.Extra.Config
import Error exposing (Error(..))


{-| Parses `String` to a `Date`, and since this parsing can fail, it returns a `Result` with a `String` error message.
Example:
    parse "%d/%m/%Y" "31/12/2017" == Date.fromString "2017-12-31"
-}
parse : Date.Extra.Config.Config -> String -> String -> Result Error Date
parse config pattern date =
    Pattern.parse pattern
        |> Result.andThen (InternalDate.parse date config)
        |> Result.andThen (InternalDate.toDate)
