module Error exposing (Error(..))

{-| DateParser Error

@docs Error

-}

import Parser


{-| DateParser Error type

An error can comes from different phases in the parsing process:

  - PatternError: an error that comes from when trying to parse the date pattern
  - ParsingError: an error that comes from parsing the date string.
  - DateError: an error when trying to construct a Date from the parsed information.

-}
type Error
    = PatternError String
    | ParsingError Parser.Error
    | DateError String
