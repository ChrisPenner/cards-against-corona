module Cards exposing (..)

import Html as H


type Color
    = White
    | Black


type alias Card =
    { color : Color
    , text : String
    }


renderCard : Card -> List (H.Html msg)
renderCard { color, text } =
    [ H.div [] [ H.text text ] ]
