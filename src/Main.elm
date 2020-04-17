port module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Assets exposing (Assets)
import Browser
import Browser.Navigation as Navigation
import Cards exposing (..)
import Debug exposing (..)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Json.Encode as E
import List.Extra as List
import List.Nonempty as Nonempty exposing (Nonempty(..))
import Player exposing (Player)
import Ports exposing (..)
import Random
import Random.List
import Result as R
import Result.Extra as R
import Url exposing (Url)
import Utils exposing (..)
import Uuid



-- MAIN


main : Program Flags AppState Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = updateApp
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest r =
    case r of
        Browser.Internal _ ->
            NoMsg

        Browser.External _ ->
            NoMsg


onUrlChange : Url.Url -> Msg
onUrlChange { path } =
    case path of
        "/" ->
            NoMsg

        _ ->
            NoMsg


subscriptions : AppState -> Sub Msg
subscriptions _ =
    Sub.batch
        [ downloadGame (decodeGame >> R.unpack (D.errorToString >> EpicFailure) DownloadGame)
        ]



-- MODEL


type ErrType
    = DecoderError D.Error


type Page
    = LandingPage
    | GamePage Game


type AppState
    = AppState
        { userID : String
        , navigationKey : Navigation.Key
        , assets : Assets
        , page : Page
        }
    | Failure String


type alias Flags =
    { userID : String
    , assets : E.Value
    }


init : Flags -> Url -> Navigation.Key -> ( AppState, Cmd Msg )
init { userID, assets } { path } key =
    case D.decodeValue Assets.decode assets of
        Err err ->
            ( Failure (D.errorToString err), Cmd.none )

        Ok decodedAssets ->
            case path of
                "/" ->
                    ( AppState
                        { navigationKey = key
                        , userID = userID
                        , assets = decodedAssets
                        , page = LandingPage
                        }
                    , Random.generate identity (Random.map StartGame newGameID)
                    )

                pth ->
                    case newGame decodedAssets userID (String.dropLeft 1 pth) of
                        Just g ->
                            ( AppState
                                { navigationKey = key
                                , userID = userID
                                , assets = decodedAssets
                                , page = LandingPage
                                }
                            , createOrJoinGameT { game = g, player = newPlayer userID }
                            )

                        Nothing ->
                            ( Failure "Not enough cards in deck to start a game", Cmd.none )


newPlayer : String -> Player
newPlayer playerID =
    { playerID = playerID
    , hand = []
    }



-- UPDATE


type Msg
    = NoMsg
    | StartGame GameID
    | DownloadGame Game
    | EpicFailure String
    | DrawCard Color
    | Reshuffle Color (Nonempty Card)
    | PlayCard Card


updateApp : Msg -> AppState -> ( AppState, Cmd Msg )
updateApp msg state =
    case state of
        Failure err ->
            ( Failure err, Cmd.none )

        AppState model ->
            case msg of
                NoMsg ->
                    ( AppState model, Cmd.none )

                EpicFailure err ->
                    ( Failure err, Cmd.none )

                StartGame gameID ->
                    case newGame model.assets model.userID gameID of
                        Just g ->
                            ( AppState model
                            , createOrJoinGameT { game = g, player = newPlayer model.userID }
                            )

                        Nothing ->
                            ( Failure "Not enough cards in deck to start a game", Cmd.none )

                DownloadGame game ->
                    ( AppState { model | page = GamePage game }
                    , case model.page of
                        LandingPage ->
                            Navigation.pushUrl model.navigationKey ("/" ++ game.gameID)

                        _ ->
                            Cmd.none
                    )

                Reshuffle Black blackDeck ->
                    case model.page of
                        GamePage g ->
                            let
                                updatedGame =
                                    { g | blackDeck = blackDeck }
                            in
                            ( AppState { model | page = GamePage updatedGame }
                            , uploadGameT updatedGame
                            )

                        _ ->
                            ( Failure "Can't shuffle on landing page", Cmd.none )

                Reshuffle White whiteDeck ->
                    case model.page of
                        GamePage g ->
                            let
                                updatedGame =
                                    { g | whiteDeck = whiteDeck }
                            in
                            ( AppState { model | page = GamePage updatedGame }
                            , uploadGameT updatedGame
                            )

                        _ ->
                            ( Failure "Can't shuffle on landing page", Cmd.none )

                DrawCard col ->
                    case ( col, model.page ) of
                        ( White, GamePage ({ whiteDeck } as game) ) ->
                            case whiteDeck of
                                -- Deck is empty, reshuffle
                                Nonempty a [] ->
                                    let
                                        updatedGame =
                                            mapPlayer model.userID (addCardToHand a) game
                                    in
                                    ( AppState { model | page = GamePage updatedGame }
                                    , Cmd.batch [ shuffleCards model.assets.whiteCards (Reshuffle White), uploadGameT updatedGame ]
                                    )

                                Nonempty a (b :: deck) ->
                                    let
                                        updatedGame =
                                            { game | whiteDeck = Nonempty b deck }
                                                |> mapPlayer model.userID (addCardToHand a)
                                    in
                                    ( AppState
                                        { model
                                            | page =
                                                GamePage
                                                    updatedGame
                                        }
                                    , uploadGameT updatedGame
                                    )

                        ( Black, GamePage ({ blackDeck, round } as game) ) ->
                            case blackDeck of
                                -- Deck is empty, reshuffle
                                Nonempty a [] ->
                                    let
                                        updatedRound =
                                            { round | blackCard = a }

                                        updatedGame =
                                            { game | round = updatedRound }
                                    in
                                    ( AppState { model | page = GamePage updatedGame }
                                    , Cmd.batch
                                        [ shuffleCards model.assets.blackCards (Reshuffle Black)
                                        , uploadGameT updatedGame
                                        ]
                                    )

                                Nonempty a (b :: deck) ->
                                    let
                                        updatedRound =
                                            { round | blackCard = a }

                                        updatedGame =
                                            { game | round = updatedRound, blackDeck = Nonempty b deck }
                                    in
                                    ( AppState { model | page = GamePage { game | blackDeck = Nonempty b deck, round = updatedRound } }
                                    , uploadGameT updatedGame
                                    )

                        _ ->
                            ( Failure "Got unexpected message on Landing page", Cmd.none )

                PlayCard card ->
                    case model.page of
                        GamePage game ->
                            let
                                g =
                                    game
                                        |> mapPlayer model.userID (removeCard card)
                                        |> addCardToRound model.userID card
                            in
                            ( AppState { model | page = GamePage g }
                            , uploadGameT g
                            )

                        _ ->
                            ( Failure "Unexpected message on landing page", Cmd.none )


addCardToRound : UserID -> Card -> Game -> Game
addCardToRound userID card ({ round } as game) =
    let
        s =
            round.submissions
                |> Dict.update userID
                    (\playedCards ->
                        case playedCards of
                            Nothing ->
                                Just <| Nonempty card []

                            Just cs ->
                                Just <| Nonempty.append cs (Nonempty card [])
                    )
    in
    { game | round = { round | submissions = s } }


mapPlayer : UserID -> (Player -> Player) -> Game -> Game
mapPlayer userID mapper game =
    { game | players = Dict.update userID (Maybe.map mapper) game.players }


removeCard : Card -> (Player -> Player)
removeCard card player =
    { player | hand = List.filter (\c -> c.text /= card.text) player.hand }


view : AppState -> Browser.Document Msg
view model =
    { title = "Cards Against Corona"
    , body = [ renderApp model ]
    }


renderApp : AppState -> H.Html Msg
renderApp state =
    case state of
        Failure err ->
            renderFailurePage err

        AppState { page, userID } ->
            case page of
                LandingPage ->
                    renderLandingPage

                GamePage g ->
                    renderGame userID g


renderLandingPage : H.Html msg
renderLandingPage =
    H.div
        []
        [ H.h1 [] [ H.text "Cards Against Corona" ]
        , H.text "Joining  Game..."
        ]


renderLoadingPage : H.Html msg
renderLoadingPage =
    H.div
        []
        [ H.h1 [] [ H.text "Cards Against Corona" ]
        , H.text "Loading  Game..."
        ]


renderFailurePage : String -> Html msg
renderFailurePage err =
    H.pre [] [ H.text err ]



--- GAME


type alias UserID =
    String


type alias GameID =
    String


renderGame : UserID -> Game -> H.Html Msg
renderGame userID { players, turn, whiteDeck, blackDeck, round } =
    H.div [ A.class "game" ]
        [ H.h1 []
            [ H.text "Cards Against Corona" ]
        , H.div [ A.class "container" ]
            [ H.a [ E.onClick (DrawCard White) ] [ renderDeck (Nonempty.toList whiteDeck) ]
            , renderCard FaceUp NoMsg Nothing round.blackCard
            , H.a [ E.onClick (DrawCard Black) ] [ renderDeck (Nonempty.toList blackDeck) ]
            ]
        , case Dict.get userID players of
            Nothing ->
                H.text "Not sure who's turn it is"

            Just { hand } ->
                H.div []
                    [ if turn == userID then
                        H.text "Your turn!"

                      else
                        H.text <| "It's " ++ turn ++ "'s turn"
                    , renderHand hand
                    , renderRound userID round
                    ]
        ]


renderDeck : List Card -> H.Html Msg
renderDeck cards =
    H.div [ A.class "deck" ] <|
        List.map (renderCard FaceDown NoMsg Nothing) cards


renderStack : CardOrientation -> List Card -> H.Html Msg
renderStack orientation cards =
    H.div [ A.class "stack" ] <|
        List.map (renderCard orientation NoMsg Nothing) cards


renderRound : UserID -> Round -> H.Html Msg
renderRound userID { submissions } =
    let
        slots =
            Dict.toList submissions
    in
    slots
        |> List.map
            (\( playerID, cards ) ->
                if playerID == userID then
                    renderStack FaceUp (Nonempty.toList cards)

                else
                    renderStack FaceDown (Nonempty.toList cards)
            )
        |> H.div [ A.class "submissions" ]


renderHand : List Card -> H.Html Msg
renderHand cards =
    let
        len =
            List.length cards
    in
    H.div [ A.class "hand" ]
        (List.indexedMap (\i card -> renderCard FaceUp (PlayCard card) (Just <| toFloat i / toFloat (len - 1)) card) cards)


type CardOrientation
    = FaceUp
    | FaceDown


renderCard : CardOrientation -> Msg -> Maybe Float -> Card -> H.Html Msg
renderCard orientation onClick percentage { color, text } =
    let
        txt =
            case orientation of
                FaceUp ->
                    text

                FaceDown ->
                    "Cards Against Humanity"

        spread =
            40.0

        rotation =
            case percentage of
                Just p ->
                    (p * spread) - (spread / 2)

                Nothing ->
                    0
    in
    H.div
        [ A.class (colorClass color)
        , A.class "card"
        , A.style "transform" ("rotate(" ++ String.fromFloat rotation ++ "deg)")
        , A.style "transform-origin" ("rotate(" ++ String.fromFloat rotation ++ "deg)")
        , E.onClick onClick
        ]
        [ H.text txt ]


createOrJoinGameT : { game : Game, player : Player } -> Cmd msg
createOrJoinGameT { game, player } =
    createOrJoinGame <|
        E.object
            [ ( "game", encodeGame game )
            , ( "player", Player.encode player )
            ]


type alias Round =
    { submissions : Dict UserID (Nonempty Card)
    , blackCard : Card
    }


type alias Game =
    { gameID : String
    , players : Dict UserID Player
    , turn : UserID
    , whiteDeck : Nonempty Card
    , blackDeck : Nonempty Card
    , round : Round
    }


newGameID : Random.Generator GameID
newGameID =
    Random.map Uuid.toString Uuid.uuidGenerator



-- decodeGame


drawHand : Nonempty Card -> ( List Card, Maybe (Nonempty Card) )
drawHand startingDeck =
    case List.splitAt 5 (Nonempty.toList startingDeck) of
        ( hand, deck ) ->
            ( hand
            , Nonempty.fromList deck
            )


newGame : Assets -> UserID -> GameID -> Maybe Game
newGame { whiteCards, blackCards } userID gameID =
    case ( drawHand whiteCards, blackCards ) of
        ( ( hand, Just whiteDeck ), Nonempty firstBlackCard (nextBlackCard :: blackDeck) ) ->
            Just
                { gameID = gameID
                , players = Dict.singleton userID { playerID = userID, hand = hand }
                , turn = userID
                , whiteDeck = whiteDeck
                , blackDeck = Nonempty nextBlackCard blackDeck
                , round =
                    { submissions = Dict.empty
                    , blackCard = firstBlackCard
                    }
                }

        _ ->
            Nothing


newRound : Assets -> Cmd Msg
newRound {} =
    Cmd.none


decodeGame : D.Value -> Result D.Error Game
decodeGame =
    D.decodeValue gameDecoder


encodeGame : Game -> E.Value
encodeGame { gameID, players, turn, blackDeck, whiteDeck, round } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        , ( "blackDeck", E.list Cards.encode <| Nonempty.toList blackDeck )
        , ( "whiteDeck", E.list Cards.encode <| Nonempty.toList whiteDeck )
        , ( "round", encodeRound round )
        ]


encodeRound : Round -> E.Value
encodeRound { submissions, blackCard } =
    E.object
        [ ( "blackCard", Cards.encode blackCard )
        , ( "submissions", E.dict identity (Nonempty.toList >> E.list Cards.encode) submissions )
        ]


gameDecoder : D.Decoder Game
gameDecoder =
    D.map6 Game
        (D.field "gameID" D.string)
        (D.field "players" (D.dict Player.decode))
        (D.field "turn" D.string)
        (D.field "whiteDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "blackDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "round" roundDecoder)


roundDecoder : D.Decoder Round
roundDecoder =
    D.map2 Round
        (D.field "submissions" (D.dict (Utils.decodeNonempty <| Cards.decode)))
        (D.field "blackCard" Cards.decode)


shuffleCards : Nonempty Card -> (Nonempty Card -> Msg) -> Cmd Msg
shuffleCards cards handler =
    Nonempty.toList cards
        |> Random.List.shuffle
        |> Random.generate identity
        |> Cmd.map
            (\cardList ->
                case cardList of
                    x :: xs ->
                        handler (Nonempty x xs)

                    _ ->
                        EpicFailure "not enough cards"
            )


addCardToHand : Card -> Player -> Player
addCardToHand c player =
    { player | hand = List.append player.hand [ c ] }



-- Outgoing


port createOrJoinGame : E.Value -> Cmd msg


port uploadGame : E.Value -> Cmd msg


uploadGameT : Game -> Cmd msg
uploadGameT game =
    uploadGame (encodeGame game)


port downloadGame : (D.Value -> msg) -> Sub msg
