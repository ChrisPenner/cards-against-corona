module Loading exposing (..)


type Loading a
    = Loading
    | Loaded a


zipLoading : (a -> b -> c) -> Loading a -> Loading b -> Loading c
zipLoading f a b =
    case ( a, b ) of
        ( Loaded la, Loaded lb ) ->
            Loaded (f la lb)

        _ ->
            Loading


toMaybe : Loading a -> Maybe a
toMaybe l =
    case l of
        Loading ->
            Nothing

        Loaded a ->
            Just a


withDefault : a -> Loading a -> a
withDefault def l =
    Maybe.withDefault def (toMaybe l)
