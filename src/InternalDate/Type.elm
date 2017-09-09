module InternalDate.Type
    exposing
        ( AmPm(..)
        , InternalDate
        , emptyDate
        )


type alias InternalDate =
    { date : Int
    , month : Int
    , year : Int
    , hour : Int
    , minute : Int
    , second : Int
    , millisecond : Int
    , ampm : Maybe AmPm
    , timeZoneOffset : Int
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
    , millisecond = 0
    , ampm = Nothing
    , timeZoneOffset = 0
    }
