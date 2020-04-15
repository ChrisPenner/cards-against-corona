module Cards exposing (..)

import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import List.Nonempty as Nonempty exposing (Nonempty)


type Color
    = White
    | Black


type alias Card =
    { color : Color
    , text : String
    }


colorClass : Color -> String
colorClass c =
    case c of
        White ->
            "white"

        Black ->
            "black"


decode : D.Decoder Card
decode =
    D.map2
        Card
        (D.field "color" D.string
            |> D.andThen
                (\col ->
                    case col of
                        "white" ->
                            D.succeed White

                        "black" ->
                            D.succeed Black

                        _ ->
                            D.fail ("Bad color " ++ col)
                )
        )
        (D.field "text" D.string)


encode : Card -> E.Value
encode { color, text } =
    E.object [ ( "text", E.string text ), ( "color", encodeColor color ) ]


encodeColor : Color -> E.Value
encodeColor c =
    E.string <|
        case c of
            White ->
                "white"

            Black ->
                "black"


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
