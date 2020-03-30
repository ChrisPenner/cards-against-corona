module Utils exposing (..)

import Json.Decode as D
import List.Nonempty exposing (Nonempty(..))


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
