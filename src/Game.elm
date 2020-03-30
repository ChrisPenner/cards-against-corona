port module Game exposing (..)

import Assets exposing (Assets)
import Cards exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import Loading exposing (..)
import Player exposing (Player)
import Random
import Uuid


type alias ID =
    String


newGameID : Random.Generator ID
newGameID =
    Random.map Uuid.toString Uuid.uuidGenerator


type alias Game =
    { gameID : String
    , players : Dict Player.ID Player
    , turn : Player.ID
    }



-- decodeGame


load : Game -> ( Game, Cmd msg )
load game =
    ( game, Cmd.batch [ requestCards () ] )


new : Player -> ID -> Game
new user gameID =
    { gameID = gameID
    , players = Dict.singleton user.playerID user
    , turn = user.playerID
    }


decode : D.Value -> Result D.Error Game
decode =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map3 Game (D.field "gameID" D.string) (D.succeed Dict.empty) (D.field "turn" D.string)


render : Player -> Assets -> Game -> H.Html msg
render { playerID } { whiteCards, blackCards } { turn } =
    H.div []
        [ H.h1 [] [ H.text "Cards Against Corona" ]
        , H.h2 []
            (if playerID == turn then
                [ H.text "Your turn!" ]

             else
                [ H.text "Waiting for your friend to play" ]
            )
        , H.div
            [ A.class "cards" ]
            (renderCards (whiteCards ++ blackCards))
        ]



-- Outgoing


createOrJoinGameT : Game -> Cmd msg
createOrJoinGameT { gameID, players, turn } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        ]
        |> createOrJoinGame


port createOrJoinGame : E.Value -> Cmd msg


port requestCards : () -> Cmd msg



-- Incoming


port joinedGame : (D.Value -> msg) -> Sub msg
