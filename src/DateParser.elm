module DateParser exposing (parse)

{-|
Custom String to Date parser. This library uses date format from https://github.com/mgold/elm-date-format/blob/1.2.0/README.md

# Parsing
@docs parse
-}

import Date exposing (Date)
import Parser exposing ((|.), (|=))


{-| Parses `String` to a `Date`, and since this parsing can fail, it returns a `Result` with a `String` error message.
Example:
    parse "%d/%m/%Y" "31/12/2017" == Date.fromString "2017-12-31"
-}
parse : String -> String -> Result String Date
parse pattern text =
    let
        parsedPattern =
            parsePattern pattern
    in
        Date.fromString text


parsePattern patter =
    Parser.succeed identity
        |= Parser.ch
