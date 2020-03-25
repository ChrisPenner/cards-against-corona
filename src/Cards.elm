module Cards exposing (..)


type Color
    = White
    | Black


type alias Card =
    { color : Color
    , text : String
    }
