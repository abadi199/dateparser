module DateParser exposing (parse)

{-|
Custom String to Date parser.
This library uses date format from https://github.com/rluiten/elm-date-extra/blob/8.2.0/DocFormat.md
and has dependency to http://package.elm-lang.org/packages/rluiten/elm-date-extra/

# Parsing
@docs parse

-}

import Pattern
import Date exposing (Date)
import InternalDate
import Date.Extra.Config
import Error exposing (Error(..))


{-| Parses `String` to a `Date`, and since this parsing can fail, it returns a `Result` with some error information.

In order to to use this function, you will need to pass `Date.Extra.Config` from this package: http://package.elm-lang.org/packages/rluiten/elm-date-extra/latest

Example:

    parse Date.Extra.Config.Config_en_us "%d/%m/%Y" "31/12/2017" == Date.fromString "2017-12-31"

-}
parse : Date.Extra.Config.Config -> String -> String -> Result Error Date
parse config pattern date =
    Pattern.parse pattern
        |> Result.andThen (InternalDate.parse date config)
        |> Result.andThen (InternalDate.toDate)
