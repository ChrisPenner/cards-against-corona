module Utils exposing (..)

import Json.Decode as D
import Json.Encode as E
import List.Nonempty as Nonempty exposing (Nonempty(..))


decodeNonempty : D.Decoder a -> D.Decoder (Nonempty a)
decodeNonempty decodeVal =
    D.list decodeVal
        |> D.andThen
            (\xs ->
                case xs of
                    x :: rest ->
                        D.succeed (Nonempty x rest)

                    _ ->
                        D.fail "expected non empty list"
            )


encodeNonempty : (a -> E.Value) -> Nonempty a -> E.Value
encodeNonempty encoder xs =
    E.list encoder <| Nonempty.toList xs
