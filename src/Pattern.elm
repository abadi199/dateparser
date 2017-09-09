module Pattern exposing (Pattern(..), other, parse)

import Error exposing (Error(..))
import Parser exposing ((|.), (|=), Parser, andThen, delayedCommit, fail, inContext, oneOf, run, succeed, symbol)


type Pattern
    = Year
    | MonthZeroPadded
    | MonthSpacePadded
    | Month
    | MonthFullName
    | MonthUpperFullName
    | MonthAbbrvName
    | MonthUpperAbbrvName
    | DateZeroPadded
    | DateSpacePadded
    | Date
    | DateSuffix
    | DateSpacePaddedSuffix
    | DayOfWeekFullName
    | DayOfWeekUpperFullName
    | DayOfWeekAbbrvName
    | DayOfWeekUpperAbbrvName
    | Hour24ZeroPadded
    | Hour24SpacePadded
    | Hour24
    | Hour12ZeroPadded
    | Hour12SpacePadded
    | Hour12
    | AMPM
    | Ampm
    | MinuteZeroPadded
    | SecondZeroPadded
    | Millisecond
    | TimeZoneOffset
    | TimeZoneOffsetColon
    | Other String


parse : String -> Result Error (List Pattern)
parse pattern =
    pattern
        |> run patternList
        |> Result.mapError (toString >> PatternError)


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
            oneOf
                [ symbol "Y" |> andThen (always <| succeed Year)
                , symbol "m" |> andThen (always <| succeed MonthZeroPadded)
                , symbol "_m" |> andThen (always <| succeed MonthSpacePadded)
                , symbol "-m" |> andThen (always <| succeed Month)
                , symbol "B" |> andThen (always <| succeed MonthFullName)
                , symbol "^B" |> andThen (always <| succeed MonthUpperFullName)
                , symbol "b" |> andThen (always <| succeed MonthAbbrvName)
                , symbol "^b" |> andThen (always <| succeed MonthUpperAbbrvName)
                , symbol "d" |> andThen (always <| succeed DateZeroPadded)
                , symbol "-d" |> andThen (always <| succeed Date)
                , symbol "-@d" |> andThen (always <| succeed DateSuffix)
                , symbol "e" |> andThen (always <| succeed DateSpacePadded)
                , symbol "@e" |> andThen (always <| succeed DateSpacePaddedSuffix)
                , symbol "A" |> andThen (always <| succeed DayOfWeekFullName)
                , symbol "^A" |> andThen (always <| succeed DayOfWeekUpperFullName)
                , symbol "a" |> andThen (always <| succeed DayOfWeekAbbrvName)
                , symbol "^a" |> andThen (always <| succeed DayOfWeekUpperAbbrvName)
                , symbol "H" |> andThen (always <| succeed Hour24ZeroPadded)
                , symbol "-H" |> andThen (always <| succeed Hour24)
                , symbol "k" |> andThen (always <| succeed Hour24SpacePadded)
                , symbol "I" |> andThen (always <| succeed Hour12ZeroPadded)
                , symbol "-I" |> andThen (always <| succeed Hour12)
                , symbol "l" |> andThen (always <| succeed Hour12SpacePadded)
                , symbol "p" |> andThen (always <| succeed AMPM)
                , symbol "P" |> andThen (always <| succeed Ampm)
                , symbol "M" |> andThen (always <| succeed MinuteZeroPadded)
                , symbol "S" |> andThen (always <| succeed SecondZeroPadded)
                , symbol "L" |> andThen (always <| succeed Millisecond)
                , symbol "z" |> andThen (always <| succeed TimeZoneOffset)
                , symbol ":z" |> andThen (always <| succeed TimeZoneOffsetColon)
                , symbol "%" |> andThen (always <| succeed (Other "%"))
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
    Parser.ignore Parser.zeroOrMore isNotSymbol
        |> Parser.sourceMap always
        |> andThen toPattern


isNotSymbol : Char -> Bool
isNotSymbol char =
    char /= '%'
