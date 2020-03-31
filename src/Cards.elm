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


renderCards : Nonempty Card -> H.Html msg
renderCards cards =
    let
        len =
            Nonempty.length cards
    in
    H.div [ A.class "cards" ]
        (Nonempty.toList <| Nonempty.indexedMap (\i card -> renderCard (toFloat i / toFloat (len - 1)) card) cards)


renderCard : Float -> Card -> H.Html msg
renderCard percentage { color, text } =
    let
        spread =
            40.0

        rotation =
            (percentage * spread) - (spread / 2)
    in
    H.div
        [ A.class (colorClass color)
        , A.class "card"
        , A.style "transform" ("rotate(" ++ String.fromFloat rotation ++ "deg)")
        , A.style "transform-origin" ("rotate(" ++ String.fromFloat rotation ++ "deg)")
        ]
        [ H.text text ]


decode : Color -> D.Decoder Card
decode color =
    D.map2 Card (D.succeed color) (D.field "text" D.string)


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
