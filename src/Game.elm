module Game exposing (..)

import Json.Decode as D
import Random
import Uuid


type alias ID =
    String


new : Random.Generator ID
new =
    Random.map Uuid.toString Uuid.uuidGenerator


type alias Game =
    { gameID : String
    }



-- decodeGame


decodeGame : D.Value -> Result D.Error Game
decodeGame =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map Game (D.field "gameID" D.string)
