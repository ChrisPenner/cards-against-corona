module Cards exposing (..)

import Html as H
import Html.Attributes as A


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
