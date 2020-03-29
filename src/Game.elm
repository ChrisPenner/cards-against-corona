port module Game exposing (..)

import Cards exposing (..)
import Debug exposing (..)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Loading exposing (..)
import Random
import Result as R
import Uuid


type alias ID =
    String


newGameID : Random.Generator ID
newGameID =
    Random.map Uuid.toString Uuid.uuidGenerator


type alias Game =
    { gameID : String
    , whiteCards : Loading (List Card)
    , blackCards : Loading (List Card)
    , errors : List String
    }


type Msg
    = LoadCards (List Card)
    | GameError String



-- decodeGame


load : ID -> ( Game, Cmd msg )
load id =
    ( new id, Cmd.batch [ requestCards () ] )


new : ID -> Game
new id =
    { gameID = id
    , whiteCards = Loading
    , blackCards = Loading
    , errors = []
    }


decodeGame : D.Value -> Result D.Error Game
decodeGame =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map4 Game (D.field "gameID" D.string) (D.succeed Loading) (D.succeed Loading) (D.succeed [])


render : Game -> List (H.Html msg)
render { whiteCards, blackCards } =
    [ H.h1 [] [ H.text "Cards Against Corona" ]
    , H.div
        [ A.class "cards" ]
        (case zipLoading List.append whiteCards blackCards of
            Loading ->
                []

            Loaded cards ->
                renderCards cards
        )
    ]


update : Msg -> Game -> ( Game, Cmd msg )
update msg g =
    case msg of
        LoadCards cards ->
            let
                ( white, black ) =
                    List.partition (\x -> x.color == White) cards
            in
            ( { g
                | whiteCards = Loaded (Loading.withDefault [] g.whiteCards ++ white)
                , blackCards = Loaded (Loading.withDefault [] g.blackCards ++ black)
              }
            , Cmd.none
            )

        GameError e ->
            ( { g | errors = e :: g.errors }, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ loadCards
            (\r ->
                case decodeCards r of
                    Ok cards ->
                        LoadCards cards

                    Err e ->
                        GameError (D.errorToString e)
            )
        ]



-- Outgoing


port createGame : ID -> Cmd msg


port requestCards : () -> Cmd msg



-- Incoming


port loadCards : (D.Value -> msg) -> Sub msg


port joinGame : (ID -> msg) -> Sub msg
