module Cards exposing (..)

import Html as H
import Html.Attributes as A
import Json.Decode as D


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


renderCards : List Card -> List (H.Html msg)
renderCards cards =
    let
        len =
            List.length cards
    in
    List.indexedMap (\i card -> renderCard (toFloat i / toFloat (len - 1)) card) cards


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
