module TestHelpers exposing (join, mapError, mapOk)


mapOk : (b -> c) -> Result a b -> Result a c
mapOk =
    Result.map


mapError : (a -> c) -> Result a b -> Result c b
mapError =
    Result.mapError


join : Result a a -> a
join result =
    case result of
        Ok ok ->
            ok

        Err error ->
            error
