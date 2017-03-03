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


{-| Parses `String` to a `Date`, and since this parsing can fail, it returns a `Result` with a `String` error message.
Example:
    parse "%d/%m/%Y" "31/12/2017" == Date.fromString "2017-12-31"
-}
parse : String -> String -> Result String Date
parse pattern date =
    let
        parsedPatternResult =
            Pattern.parse pattern
    in
        case parsedPatternResult of
            Ok parsedPattern ->
                Err "not implemented yet"

            Err error ->
                Err error
