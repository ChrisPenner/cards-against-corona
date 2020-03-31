module Player exposing (..)

import Cards exposing (Card)
import Json.Decode as D
import Json.Encode as E
import List.Nonempty as Nonempty exposing (Nonempty)
import Utils


type alias ID =
    String


type alias Player =
    { playerID : ID
    , hand : Nonempty Card
    }


encode : Player -> E.Value
encode { playerID, hand } =
    E.object
        [ ( "playerID", E.string playerID )
        , ( "hand", Utils.encodeNonempty Cards.encode hand )
        ]


decode : D.Decoder Player
decode =
    D.map2 Player
        (D.field "playerID" <| D.string)
        (D.field "hand" <| Utils.decodeNonempty <| Cards.decode Cards.White)
