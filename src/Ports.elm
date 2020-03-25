port module Ports exposing (..)

import Cards exposing (..)
import Json.Decode as D


port loadCards : (D.Value -> msg) -> Sub msg


decodeCards : D.Value -> Result D.Error (List Card)
decodeCards =
    D.decodeValue (D.list cardDecoder)


cardDecoder : D.Decoder Card
cardDecoder =
    D.map2 Card colorDecoder (D.field "text" D.string)


colorDecoder : D.Decoder Color
colorDecoder =
    D.field "color" D.string
        |> D.andThen
            (\colorText ->
                case colorText of
                    "white" ->
                        D.succeed White

                    "black" ->
                        D.succeed Black

                    s ->
                        D.fail ("Unknown color type: " ++ s)
            )
