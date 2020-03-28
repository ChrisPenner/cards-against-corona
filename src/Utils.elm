module Utils exposing (..)


wrapUpdate : (msg -> state -> ( state, Cmd msg )) -> Maybe msg -> state -> ( state, Cmd (Maybe msg) )
wrapUpdate updater msg s =
    case msg of
        Just m ->
            Tuple.mapSecond (Cmd.map Just) (updater m s)

        Nothing ->
            ( s, Cmd.none )
