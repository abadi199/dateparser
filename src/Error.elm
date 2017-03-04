module Error exposing (Error(..))

{-|
DateParser Error

@docs Error
-}

import Parser


{-| DateParser Error type
-}
type Error
    = PatternError String
    | ParsingError Parser.Error
    | DateError String
