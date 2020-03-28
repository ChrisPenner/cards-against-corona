port module Ports exposing (..)

import Cards exposing (..)
import Game
import Json.Decode as D
import Json.Encode as E


port createGame : Game.ID -> Cmd msg


port joinGame : (D.Value -> msg) -> Sub msg


port loadCards : (D.Value -> msg) -> Sub msg
