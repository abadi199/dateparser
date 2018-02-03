module InternalDate.Parser exposing (..)

import Date exposing (Day(..), Month(..))
import Date.Extra.Config
import InternalDate.Type exposing (AmPm(..), InternalDate)
import Parser exposing ((|.), (|=), Error, Parser, inContext, oneOrMore, repeat, succeed)
import Pattern exposing (Pattern(..))
import String
import Utilities exposing (monthToInt, toUpper)


fromPattern : Date.Extra.Config.Config -> Pattern -> (InternalDate -> Parser InternalDate)
fromPattern config pattern =
    case pattern of
        Year ->
            year

        MonthZeroPadded ->
            monthZeroPadded

        MonthSpacePadded ->
            monthSpacePadded

        Month ->
            month

        MonthFullName ->
            monthName { isFullName = True, isUpper = False, dateConfig = config }

        MonthUpperFullName ->
            monthName { isFullName = True, isUpper = True, dateConfig = config }

        MonthAbbrvName ->
            monthName { isFullName = False, isUpper = False, dateConfig = config }

        MonthUpperAbbrvName ->
            monthName { isFullName = False, isUpper = True, dateConfig = config }

        DateZeroPadded ->
            dateZeroPadded

        DateSpacePadded ->
            dateSpacePadded

        Date ->
            date

        DateSuffix ->
            dateSuffix False config

        DateSpacePaddedSuffix ->
            dateSuffix True config

        DayOfWeekFullName ->
            dayOfWeek { isFullName = True, isUpper = False, dateConfig = config }

        DayOfWeekUpperFullName ->
            dayOfWeek { isFullName = True, isUpper = True, dateConfig = config }

        DayOfWeekAbbrvName ->
            dayOfWeek { isFullName = False, isUpper = False, dateConfig = config }

        DayOfWeekUpperAbbrvName ->
            dayOfWeek { isFullName = False, isUpper = True, dateConfig = config }

        Hour24ZeroPadded ->
            hour24ZeroPadded

        Hour24SpacePadded ->
            hour24SpacePadded

        Hour24 ->
            hour24

        Hour12ZeroPadded ->
            hour12ZeroPadded

        Hour12SpacePadded ->
            hour12SpacePadded

        Hour12 ->
            hour12

        AMPM ->
            aMPM

        Ampm ->
            ampm

        MinuteZeroPadded ->
            minuteZeroPadded

        SecondZeroPadded ->
            secondZeroPadded

        Millisecond ->
            millisecond

        TimeZoneOffset ->
            timeZoneOffset False

        TimeZoneOffsetColon ->
            timeZoneOffset True

        Whitespace ->
            whitespace

        Other symbol ->
            other symbol


aMPM : InternalDate -> Parser InternalDate
aMPM internalDate =
    inContext "AMPM" <|
        Parser.oneOf
            [ Parser.symbol "AM" |> Parser.andThen (\_ -> succeed { internalDate | ampm = Just AM })
            , Parser.symbol "PM" |> Parser.andThen (\_ -> succeed { internalDate | ampm = Just PM })
            ]


ampm : InternalDate -> Parser InternalDate
ampm internalDate =
    inContext "ampm" <|
        Parser.oneOf
            [ Parser.symbol "am" |> Parser.andThen (\_ -> succeed { internalDate | ampm = Just AM })
            , Parser.symbol "pm" |> Parser.andThen (\_ -> succeed { internalDate | ampm = Just PM })
            ]


year : InternalDate -> Parser InternalDate
year internalDate =
    (inContext "Year" <| succeed identity |= Parser.int)
        |> Parser.andThen (\year -> succeed { internalDate | year = year })


secondZeroPadded : InternalDate -> Parser InternalDate
secondZeroPadded =
    inContext "SecondZeroPadded" << secondPadded "0"


secondPadded : String -> InternalDate -> Parser InternalDate
secondPadded pad internalDate =
    List.range 0 59
        |> List.map (paddedInt pad (\number -> { internalDate | second = number }))
        |> Parser.oneOf


minuteZeroPadded : InternalDate -> Parser InternalDate
minuteZeroPadded =
    inContext "MinuteZeroPadded" << minutePadded "0"


minutePadded : String -> InternalDate -> Parser InternalDate
minutePadded pad internalDate =
    List.range 0 59
        |> List.map (paddedInt pad (\number -> { internalDate | minute = number }))
        |> Parser.oneOf


hour12 : InternalDate -> Parser InternalDate
hour12 internalDate =
    Parser.int
        |> Parser.andThen
            (\hour ->
                if hour < 13 then
                    Parser.succeed { internalDate | hour = hour }
                else
                    Parser.fail "not a valid 12 hour"
            )
        |> inContext "Hour12"


hour12ZeroPadded : InternalDate -> Parser InternalDate
hour12ZeroPadded =
    inContext "Hour12ZeroPadded" << hour12Padded "0"


hour12SpacePadded : InternalDate -> Parser InternalDate
hour12SpacePadded =
    inContext "Hour12SpacePadded" << hour12Padded " "


hour12Padded : String -> InternalDate -> Parser InternalDate
hour12Padded pad internalDate =
    List.range 1 12
        |> List.map (paddedInt pad (\number -> { internalDate | hour = number }))
        |> Parser.oneOf


hour24ZeroPadded : InternalDate -> Parser InternalDate
hour24ZeroPadded =
    inContext "Hour24ZeroPadded" << hour24Padded "0"


hour24SpacePadded : InternalDate -> Parser InternalDate
hour24SpacePadded =
    inContext "Hour24SpacePadded" << hour24Padded " "


hour24 : InternalDate -> Parser InternalDate
hour24 internalDate =
    Parser.int
        |> Parser.andThen
            (\hour ->
                if hour < 25 then
                    Parser.succeed { internalDate | hour = hour }
                else
                    Parser.fail "not a valid 24 hour"
            )
        |> inContext "Hour24"


hour24Padded : String -> InternalDate -> Parser InternalDate
hour24Padded pad internalDate =
    List.range 0 23
        |> List.map (paddedInt pad (\number -> { internalDate | hour = number }))
        |> Parser.oneOf


monthZeroPadded : InternalDate -> Parser InternalDate
monthZeroPadded =
    inContext "MonthZeroPadded" << monthPadded "0"


monthSpacePadded : InternalDate -> Parser InternalDate
monthSpacePadded =
    inContext "MonthSpacePadded" << monthPadded " "


month : InternalDate -> Parser InternalDate
month internalDate =
    Parser.oneOf
        (List.range 1 12 |> List.map (number (\int -> { internalDate | month = int })))


monthPadded : String -> InternalDate -> Parser InternalDate
monthPadded pad internalDate =
    Parser.oneOf
        (List.range 1 12 |> List.map (paddedInt pad (\number -> { internalDate | month = number })))


type alias ParserConfig =
    { isUpper : Bool, isFullName : Bool, dateConfig : Date.Extra.Config.Config }


monthNameParser : ParserConfig -> InternalDate -> Date.Month -> Parser InternalDate
monthNameParser config internalDate month =
    let
        getMonthName =
            if config.isFullName then
                config.dateConfig.i18n.monthName
            else
                config.dateConfig.i18n.monthShort
    in
        Parser.symbol (getMonthName month |> toUpper config.isUpper) |> Parser.andThen (always <| succeed { internalDate | month = monthToInt month })


monthName : ParserConfig -> InternalDate -> Parser InternalDate
monthName config internalDate =
    [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
        |> List.map (monthNameParser config internalDate)
        |> Parser.oneOf
        |> inContext "MonthName"


dayNameParser : ParserConfig -> InternalDate -> Date.Day -> Parser InternalDate
dayNameParser config internalDate day =
    let
        getDayName =
            if config.isFullName then
                config.dateConfig.i18n.dayName
            else
                config.dateConfig.i18n.dayShort
    in
        Parser.symbol (getDayName day |> toUpper config.isUpper) |> Parser.andThen (always <| succeed internalDate)


dayOfWeek : ParserConfig -> InternalDate -> Parser InternalDate
dayOfWeek config internalDate =
    [ Sun, Mon, Tue, Wed, Thu, Fri, Sat ]
        |> List.map (dayNameParser config internalDate)
        |> Parser.oneOf
        |> inContext "DayOfWeek"


dateZeroPadded : InternalDate -> Parser InternalDate
dateZeroPadded =
    datePadded "0"


dateSpacePadded : InternalDate -> Parser InternalDate
dateSpacePadded =
    datePadded " "


date : InternalDate -> Parser InternalDate
date internalDate =
    Parser.int
        |> Parser.andThen
            (\number ->
                if number <= 31 then
                    Parser.succeed { internalDate | date = number }
                else
                    Parser.fail "not a valid date"
            )
        |> inContext "date"


datePadded : String -> InternalDate -> Parser InternalDate
datePadded pad internalDate =
    inContext "DateZeroPadded" <|
        Parser.oneOf
            (List.range 1 31 |> List.map (paddedInt pad (\number -> { internalDate | date = number })))


dateSuffix : Bool -> Date.Extra.Config.Config -> InternalDate -> Parser InternalDate
dateSuffix usePadding config internalDate =
    let
        parser date =
            Parser.symbol (config.i18n.dayOfMonthWithSuffix usePadding date) |> Parser.andThen (\_ -> Parser.succeed { internalDate | date = date })
    in
        Parser.oneOf
            (List.range 1 31 |> List.map parser)


number : (Int -> InternalDate) -> Int -> Parser InternalDate
number f number =
    Parser.symbol (toString number) |> Parser.andThen (always <| succeed (f number))


paddedInt : String -> (Int -> InternalDate) -> Int -> Parser InternalDate
paddedInt pad f number =
    let
        numberStr =
            toString number

        paddedNumber =
            if String.length numberStr == 1 then
                pad ++ numberStr
            else
                numberStr
    in
        if String.length numberStr > 2 then
            Parser.fail "Number is more than 2 digit"
        else
            Parser.symbol paddedNumber |> Parser.andThen (always <| succeed (f number))


millisecond : InternalDate -> Parser InternalDate
millisecond internalDate =
    let
        thirdDigit ms =
            if ms < 10 then
                Parser.succeed { internalDate | millisecond = ms }
            else
                Parser.fail "not millisecond"

        secondDigit ms =
            if ms < 100 then
                Parser.succeed { internalDate | millisecond = ms }
            else
                Parser.fail "not millisecond"

        firstDigit ms =
            if ms < 1000 then
                Parser.succeed { internalDate | millisecond = ms }
            else
                Parser.fail "not millisecond"
    in
        Parser.oneOf
            [ Parser.delayedCommit (Parser.symbol "00") Parser.int |> Parser.andThen thirdDigit
            , Parser.delayedCommit (Parser.symbol "0") Parser.int |> Parser.andThen secondDigit
            , Parser.int |> Parser.andThen firstDigit
            ]


timeZoneOffset : Bool -> InternalDate -> Parser InternalDate
timeZoneOffset useColon internalDate =
    let
        parser : Parser ( Int, Int )
        parser =
            if useColon then
                Parser.succeed (,)
                    |= paddedHour
                    |. Parser.symbol ":"
                    |= paddedMinute
            else
                Parser.succeed (,)
                    |= paddedHour
                    |= paddedMinute

        updateInternalDate sign ( hour, minute ) =
            Parser.succeed { internalDate | timeZoneOffset = sign * ((hour * 60) + minute) }
    in
        Parser.oneOf
            [ Parser.delayedCommit (Parser.symbol "+") parser |> Parser.andThen (updateInternalDate -1)
            , Parser.delayedCommit (Parser.symbol "-") parser |> Parser.andThen (updateInternalDate 1)
            ]


paddedInts : List Int -> Parser Int
paddedInts numbers =
    let
        pad number =
            if number < 10 then
                "0" ++ toString number
            else
                toString number

        parser number =
            Parser.symbol (pad number) |> Parser.andThen (always (Parser.succeed number))
    in
        numbers
            |> List.map parser
            |> Parser.oneOf


paddedHour : Parser Int
paddedHour =
    List.range 0 12
        |> paddedInts


paddedMinute : Parser Int
paddedMinute =
    List.range 0 59
        |> paddedInts


timeZoneOffsetColon : InternalDate -> Parser InternalDate
timeZoneOffsetColon internalDate =
    Debug.crash "timeZoneOffsetColon"


whitespace : InternalDate -> Parser InternalDate
whitespace internalDate =
    (inContext "Whitespace" <| succeed identity |. repeat oneOrMore (Parser.symbol " "))
        |> Parser.andThen (\_ -> succeed internalDate)


other : String -> InternalDate -> Parser InternalDate
other symbol internalDate =
    (inContext "Other" <| succeed identity |. Parser.symbol symbol)
        |> Parser.andThen (\_ -> succeed internalDate)
