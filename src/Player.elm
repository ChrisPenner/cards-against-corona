module Player exposing (..)

import Json.Encode as E


type alias ID =
    String


type alias Player =
    { playerID : ID }


encode : Player -> E.Value
encode {} =
    E.object []
