module DateParser exposing (parse)

{-| Custom String to Date parser.

This library required <http://package.elm-lang.org/packages/rluiten/elm-date-extra/> package.


# Parsing

@docs parse

-}

import Date exposing (Date)
import Date.Extra.Config
import Error exposing (Error(..))
import InternalDate
import Pattern


{-| Parses `String` to a `Date`, and since this parsing can fail, it returns a `Result` with some error information.

In order to to use this function, you will need to pass `Date.Extra.Config` from this package: <http://package.elm-lang.org/packages/rluiten/elm-date-extra/latest>

Example:

    parse Date.Extra.Config.Config_en_us "%d/%m/%Y" "31/12/2017" == Date.fromString "2017-12-31"

-}
parse : Date.Extra.Config.Config -> String -> String -> Result Error Date
parse config pattern date =
    Pattern.parse pattern
        |> Result.andThen (InternalDate.parse date config)
        |> Result.andThen InternalDate.toDate
