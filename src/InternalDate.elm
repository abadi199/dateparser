module InternalDate exposing (InternalDate, parse, emptyDate, AmPm(..))

import Pattern exposing (Pattern(..))
import Parser exposing (Error, Parser, succeed, (|=), (|.), inContext)
import String


type alias InternalDate =
    { date : Int
    , month : Int
    , year : Int
    , hour : Int
    , minute : Int
    , second : Int
    , ampm : Maybe AmPm
    }


type AmPm
    = AM
    | PM


emptyDate : InternalDate
emptyDate =
    { date = 1
    , month = 1
    , year = 1900
    , hour = 0
    , minute = 0
    , second = 0
    , ampm = Nothing
    }



-- , hour : Int
-- , minute : Int
-- , second : Int
-- , timeOffset : Int
-- }


parse : List Pattern -> String -> Result Parser.Error InternalDate
parse patterns str =
    let
        parser =
            patterns
                |> List.map fromPattern
                |> List.foldl (\a b -> b |> Parser.andThen a) (succeed emptyDate)
    in
        Parser.run parser str


fromPattern : Pattern -> (InternalDate -> Parser InternalDate)
fromPattern pattern =
    case pattern of
        Year ->
            year

        MonthZeroPadded ->
            monthZeroPadded

        MonthFullName ->
            monthFullName

        MonthAbbrvName ->
            monthAbbrvName

        DateZeroPadded ->
            dateZeroPadded

        DateSpacePadded ->
            dateSpacePadded

        Hour24ZeroPadded ->
            hour24ZeroPadded

        Hour24SpacePadded ->
            hour24SpacePadded

        Hour12ZeroPadded ->
            hour12ZeroPadded

        Hour12SpacePadded ->
            hour12SpacePadded

        AMPM ->
            aMPM

        Ampm ->
            ampm

        MinuteZeroPadded ->
            minuteZeroPadded

        SecondZeroPadded ->
            secondZeroPadded

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
    List.range 1 60
        |> List.map (paddedInt pad (\number -> { internalDate | second = number }))
        |> Parser.oneOf


minuteZeroPadded : InternalDate -> Parser InternalDate
minuteZeroPadded =
    inContext "MinuteZeroPadded" << minutePadded "0"


minutePadded : String -> InternalDate -> Parser InternalDate
minutePadded pad internalDate =
    List.range 1 60
        |> List.map (paddedInt pad (\number -> { internalDate | minute = number }))
        |> Parser.oneOf


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


hour24Padded : String -> InternalDate -> Parser InternalDate
hour24Padded pad internalDate =
    List.range 1 24
        |> List.map (paddedInt pad (\number -> { internalDate | hour = number }))
        |> Parser.oneOf


monthZeroPadded : InternalDate -> Parser InternalDate
monthZeroPadded =
    inContext "MonthZeroPadded" << monthPadded "0"


monthSpacePadded : InternalDate -> Parser InternalDate
monthSpacePadded =
    inContext "MonthSpacePadded" << monthPadded " "


monthPadded : String -> InternalDate -> Parser InternalDate
monthPadded pad internalDate =
    Parser.oneOf
        (List.range 1 12 |> List.map (paddedInt pad (\number -> { internalDate | month = number })))


monthFullName : InternalDate -> Parser InternalDate
monthFullName internalDate =
    inContext "MonthFullName" <|
        Parser.oneOf
            [ Parser.symbol "January" |> Parser.andThen (always <| succeed { internalDate | month = 1 })
            , Parser.symbol "February" |> Parser.andThen (always <| succeed { internalDate | month = 2 })
            , Parser.symbol "March" |> Parser.andThen (always <| succeed { internalDate | month = 3 })
            , Parser.symbol "April" |> Parser.andThen (always <| succeed { internalDate | month = 4 })
            , Parser.symbol "May" |> Parser.andThen (always <| succeed { internalDate | month = 5 })
            , Parser.symbol "June" |> Parser.andThen (always <| succeed { internalDate | month = 6 })
            , Parser.symbol "July" |> Parser.andThen (always <| succeed { internalDate | month = 7 })
            , Parser.symbol "August" |> Parser.andThen (always <| succeed { internalDate | month = 8 })
            , Parser.symbol "September" |> Parser.andThen (always <| succeed { internalDate | month = 9 })
            , Parser.symbol "October" |> Parser.andThen (always <| succeed { internalDate | month = 10 })
            , Parser.symbol "November" |> Parser.andThen (always <| succeed { internalDate | month = 11 })
            , Parser.symbol "December" |> Parser.andThen (always <| succeed { internalDate | month = 12 })
            ]


monthAbbrvName : InternalDate -> Parser InternalDate
monthAbbrvName internalDate =
    inContext "MonthAbbrvName" <|
        Parser.oneOf
            [ Parser.symbol "Jan" |> Parser.andThen (always <| succeed { internalDate | month = 1 })
            , Parser.symbol "Feb" |> Parser.andThen (always <| succeed { internalDate | month = 2 })
            , Parser.symbol "Mar" |> Parser.andThen (always <| succeed { internalDate | month = 3 })
            , Parser.symbol "Apr" |> Parser.andThen (always <| succeed { internalDate | month = 4 })
            , Parser.symbol "May" |> Parser.andThen (always <| succeed { internalDate | month = 5 })
            , Parser.symbol "Jun" |> Parser.andThen (always <| succeed { internalDate | month = 6 })
            , Parser.symbol "Jul" |> Parser.andThen (always <| succeed { internalDate | month = 7 })
            , Parser.symbol "Aug" |> Parser.andThen (always <| succeed { internalDate | month = 8 })
            , Parser.symbol "Sep" |> Parser.andThen (always <| succeed { internalDate | month = 9 })
            , Parser.symbol "Oct" |> Parser.andThen (always <| succeed { internalDate | month = 10 })
            , Parser.symbol "Nov" |> Parser.andThen (always <| succeed { internalDate | month = 11 })
            , Parser.symbol "Dec" |> Parser.andThen (always <| succeed { internalDate | month = 12 })
            ]


dateZeroPadded : InternalDate -> Parser InternalDate
dateZeroPadded =
    datePadded "0"


dateSpacePadded : InternalDate -> Parser InternalDate
dateSpacePadded =
    datePadded " "


datePadded : String -> InternalDate -> Parser InternalDate
datePadded pad internalDate =
    inContext "DateZeroPadded" <|
        Parser.oneOf
            (List.range 1 31 |> List.map (paddedInt pad (\number -> { internalDate | date = number })))


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


other : String -> InternalDate -> Parser InternalDate
other symbol internalDate =
    (inContext "Other" <| succeed identity |. Parser.symbol symbol)
        |> Parser.andThen (\_ -> succeed internalDate)
