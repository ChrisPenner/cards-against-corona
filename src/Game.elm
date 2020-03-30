port module Game exposing (..)

import Assets exposing (Assets)
import Cards exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import List.Nonempty as Nonempty
import Loading exposing (..)
import Player exposing (Player)
import Random
import Random.List as Random
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
    , blackCard : Card
    , seed : Random.Seed
    }


type Msg
    = NewRound Card



-- decodeGame


load : Game -> ( Game, Cmd msg )
load game =
    ( game, Cmd.none )


new : Assets -> Player -> ID -> Random.Seed -> Game
new assets user gameID seed =
    { gameID = gameID
    , players = Dict.singleton user.playerID user
    , turn = user.playerID
    , blackCard = { color = Black, text = "" }
    , seed = seed
    }


pickCard : Random.Seed -> List Card -> { seed : Random.Seed, card : Card }
pickCard seed cards =
    Random.step (Nonempty.sample cards)


newRound : Assets -> Cmd Msg
newRound {} =
    Cmd.none


decode : D.Value -> Result D.Error Game
decode =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map5 Game
        (D.field "gameID" D.string)
        (D.succeed Dict.empty)
        (D.field "turn" D.string)
        (D.field "blackCard" <| Cards.decode Black)
        (D.field "seed" <| D.map Random.initialSeed D.int)


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
            (renderCards whiteCards)
        ]



-- Outgoing


createOrJoinGameT : Game -> Cmd msg
createOrJoinGameT { gameID, players, turn, blackCard, seed } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        , ( "blackCard", Cards.encode blackCard )
        , ( "seed", E.int seed )
        ]
        |> createOrJoinGame


port createOrJoinGame : E.Value -> Cmd msg



-- Incoming


port joinedGame : (D.Value -> msg) -> Sub msg
