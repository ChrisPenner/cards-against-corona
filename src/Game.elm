port module Game exposing (..)

import Assets exposing (Assets)
import Cards exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Loading exposing (..)
import Player exposing (Player)
import Random
import Random.List as Random
import Utils
import Uuid


type alias UserID =
    String


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
    , whiteDeck : Nonempty Card
    , blackDeck : Nonempty Card
    }


type Msg
    = NewRound Card



-- decodeGame


load : Game -> ( Game, Cmd msg )
load game =
    ( game, Cmd.none )


drawHand : Nonempty Card -> ( Maybe (Nonempty Card), Maybe (Nonempty Card) )
drawHand startingDeck =
    case List.splitAt 5 (Nonempty.toList startingDeck) of
        ( hand, deck ) ->
            ( Nonempty.fromList hand
            , Nonempty.fromList deck
            )


new : Assets -> UserID -> ID -> Maybe Game
new { whiteCards, blackCards } userID gameID =
    case drawHand whiteCards of
        ( Just hand, Just deck ) ->
            Just
                { gameID = gameID
                , players = Dict.singleton userID { playerID = userID, hand = hand }
                , turn = userID
                , blackCard = { color = Black, text = "" }
                , whiteDeck = whiteCards
                , blackDeck = blackCards
                }

        _ ->
            Nothing


newRound : Assets -> Cmd Msg
newRound {} =
    Cmd.none


decode : D.Value -> Result D.Error Game
decode =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map6 Game
        (D.field "gameID" D.string)
        (D.field "players" (D.dict Player.decode))
        (D.field "turn" D.string)
        (D.field "blackCard" <| Cards.decode Black)
        (D.field "blackDeck" <| Utils.decodeNonempty <| Cards.decode Black)
        (D.field "whiteDeck" <| Utils.decodeNonempty <| Cards.decode White)


render : UserID -> Game -> H.Html msg
render userID { players, turn } =
    H.div []
        [ H.h1 []
            [ H.text "Cards Against Corona" ]
        , case Dict.get userID players of
            Nothing ->
                H.text "Not sure who's turn it is"

            Just { hand } ->
                H.div []
                    [ if turn == userID then
                        H.text "Your turn!"

                      else
                        H.text <| "It's " ++ turn ++ "'s turn"
                    , renderCards hand
                    ]
        ]


createOrJoinGameT : Game -> Cmd msg
createOrJoinGameT { gameID, players, turn, blackCard, blackDeck, whiteDeck } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        , ( "blackCard", Cards.encode blackCard )
        , ( "blackDeck", E.list Cards.encode <| Nonempty.toList blackDeck )
        , ( "whiteDeck", E.list Cards.encode <| Nonempty.toList whiteDeck )
        ]
        |> createOrJoinGame


port createOrJoinGame : E.Value -> Cmd msg



-- Incoming


port joinedGame : (D.Value -> msg) -> Sub msg
