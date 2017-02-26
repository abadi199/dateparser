module InternalDate exposing (InternalDate)

import Pattern exposing (Pattern)
import Parser exposing (Error)


type alias InternalDate =
    { date : Int
    , month : Int
    , year : Int
    }



-- , hour : Int
-- , minute : Int
-- , second : Int
-- , timeOffset : Int
-- }


parse : List Pattern -> String -> Result String InternalDate
parse patterns str =
    Ok { date = 1, month = 1, year = 2000 }
