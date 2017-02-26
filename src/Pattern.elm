module Pattern exposing (parse, Pattern(..), other)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, andThen, oneOf, delayedCommit, inContext, run, Error, fail)


type Pattern
    = Year
    | MonthZeroPadded
    | MonthFullName
    | MonthAbbrvName
    | DateZeroPadded
    | DateSpacePaddedd
    | Hour24ZerroPadded
    | Hour24SpacePadded
    | Hour12ZeroPadded
    | Hour12SpacePadded
    | AMPM
    | Ampm
    | MinuteZeroPadded
    | SecondZeroPadded
    | Other String


parse : String -> Result String (List Pattern)
parse pattern =
    pattern
        |> run patternList
        |> Result.mapError (toString)
        |> Result.andThen checkDuplicate


{-| TODO: optimize this
-}
checkDuplicate : List Pattern -> Result String (List Pattern)
checkDuplicate patterns =
    let
        moreThanTwo list =
            (List.length list) >= 2

        monthFilter pattern =
            pattern == MonthZeroPadded || pattern == MonthAbbrvName || pattern == MonthFullName

        dateFilter pattern =
            pattern == DateSpacePaddedd || pattern == DateZeroPadded
    in
        if patterns |> List.filter monthFilter |> moreThanTwo then
            Err "Invalid pattern: duplicate month"
        else if patterns |> List.filter dateFilter |> moreThanTwo then
            Err "Invalid pattern: duplicate date"
        else
            Ok patterns


patternList : Parser (List Pattern)
patternList =
    inContext "patternList" <|
        succeed identity
            |= patternListHelper []


patternListHelper : List Pattern -> Parser (List Pattern)
patternListHelper patterns =
    oneOf
        [ nextPattern |> andThen (\pattern -> patternListHelper (pattern :: patterns))
        , other |> andThen (\pattern -> patternListHelper (pattern :: patterns))
        , succeed (List.reverse patterns)
        ]


nextPattern : Parser Pattern
nextPattern =
    inContext "pattern" <|
        delayedCommit (symbol "%") <|
            succeed identity
                |= oneOf
                    [ symbol "Y" |> andThen (always <| succeed Year)
                    , symbol "m" |> andThen (always <| succeed MonthZeroPadded)
                    , symbol "B" |> andThen (always <| succeed MonthFullName)
                    , symbol "b" |> andThen (always <| succeed MonthAbbrvName)
                    , symbol "d" |> andThen (always <| succeed DateZeroPadded)
                    , symbol "e" |> andThen (always <| succeed DateSpacePaddedd)
                    , symbol "H" |> andThen (always <| succeed Hour24ZerroPadded)
                    , symbol "k" |> andThen (always <| succeed Hour24SpacePadded)
                    , symbol "I" |> andThen (always <| succeed Hour12ZeroPadded)
                    , symbol "l" |> andThen (always <| succeed Hour12SpacePadded)
                    , symbol "p" |> andThen (always <| succeed AMPM)
                    , symbol "P" |> andThen (always <| succeed Ampm)
                    , symbol "M" |> andThen (always <| succeed MinuteZeroPadded)
                    , symbol "S" |> andThen (always <| succeed SecondZeroPadded)
                    ]


other : Parser Pattern
other =
    let
        toPattern str =
            if str == "" then
                fail "empty character"
            else
                succeed (Other str)
    in
        Parser.ignoreWhile isNotSymbol
            |> Parser.mapWithSource always
            |> andThen toPattern


isNotSymbol : Char -> Bool
isNotSymbol char =
    char /= '%'
