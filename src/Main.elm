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
    | FlipSubmission UserID Card


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
                    state
                        |> mapGame (\g -> { g | blackDeck = blackDeck })
                        |> (\s -> ( s, uploadGameT s ))

                Reshuffle White whiteDeck ->
                    state
                        |> mapGame (\g -> { g | whiteDeck = whiteDeck })
                        |> (\s -> ( s, uploadGameT s ))

                DrawCard col ->
                    state
                        |> traverseGame
                            (\game ->
                                case col of
                                    White ->
                                        case game.whiteDeck of
                                            -- Deck is empty, reshuffle
                                            Nonempty a [] ->
                                                ( mapPlayer model.userID (addCardToHand a) game
                                                , shuffleCards model.assets.whiteCards (Reshuffle White)
                                                )

                                            Nonempty a (b :: deck) ->
                                                ( { game | whiteDeck = Nonempty b deck } |> mapPlayer model.userID (addCardToHand a)
                                                , Cmd.none
                                                )

                                    Black ->
                                        case game.blackDeck of
                                            -- Deck is empty, reshuffle
                                            Nonempty a [] ->
                                                ( mapRound (\r -> { r | blackCard = a }) game
                                                , shuffleCards model.assets.blackCards (Reshuffle Black)
                                                )

                                            Nonempty a (b :: deck) ->
                                                ( game |> mapRound (\r -> { r | blackCard = a }) |> (\g -> { g | blackDeck = Nonempty b deck })
                                                , Cmd.none
                                                )
                            )
                        |> (\( updatedState, cmds ) ->
                                ( updatedState, Cmd.batch [ cmds, uploadGameT updatedState ] )
                           )

                PlayCard card ->
                    state
                        |> mapGame
                            (mapPlayer model.userID (removeCard card)
                                >> addCardToRound model.userID card
                            )
                        |> (\s -> ( s, uploadGameT s ))

                FlipSubmission playerID card ->
                    state
                        |> (mapGame << mapSubmission playerID <| flipCard card)
                        |> (\s -> ( s, uploadGameT s ))


mapGame : (Game -> Game) -> AppState -> AppState
mapGame mapper state =
    case state of
        AppState ({ page } as model) ->
            case page of
                GamePage g ->
                    AppState { model | page = GamePage (mapper g) }

                _ ->
                    state

        _ ->
            state


traverseGame : (Game -> ( Game, Cmd a )) -> AppState -> ( AppState, Cmd a )
traverseGame mapper state =
    case state of
        AppState ({ page } as model) ->
            case page of
                GamePage g ->
                    case mapper g of
                        ( newG, cmds ) ->
                            ( AppState { model | page = GamePage newG }, cmds )

                _ ->
                    ( state, Cmd.none )

        _ ->
            ( state, Cmd.none )


flipCard : Card -> Nonempty Submission -> Nonempty Submission
flipCard { text } =
    Nonempty.map
        (\({ card, orientation } as sub) ->
            if card.text == text then
                { sub | orientation = flipOrientation orientation }

            else
                sub
        )


flipOrientation : Orientation -> Orientation
flipOrientation ori =
    case ori of
        FaceUp ->
            FaceDown

        FaceDown ->
            FaceUp


addCardToRound : UserID -> Card -> Game -> Game
addCardToRound userID card ({ round } as game) =
    let
        s =
            round.submissions
                |> Dict.update userID
                    (\playedCards ->
                        case playedCards of
                            Nothing ->
                                Just <| Nonempty { card = card, orientation = FaceDown } []

                            Just cs ->
                                Just <| Nonempty.append cs (Nonempty { card = card, orientation = FaceDown } [])
                    )
    in
    { game | round = { round | submissions = s } }


mapPlayer : UserID -> (Player -> Player) -> Game -> Game
mapPlayer userID mapper game =
    { game | players = Dict.update userID (Maybe.map mapper) game.players }


mapRound : (Round -> Round) -> Game -> Game
mapRound mapper ({ round } as game) =
    { game | round = mapper round }


mapSubmissions : (Dict UserID (Nonempty Submission) -> Dict UserID (Nonempty Submission)) -> Round -> Round
mapSubmissions mapper ({ submissions } as round) =
    { round | submissions = mapper submissions }


mapSubmission : UserID -> (Nonempty Submission -> Nonempty Submission) -> Game -> Game
mapSubmission userID mapper game =
    game
        |> (mapRound << mapSubmissions << Dict.update userID << Maybe.map) mapper


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
            , H.map (always NoMsg) <| renderCard FaceUp Nothing round.blackCard
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
        List.map (H.map (always NoMsg) << renderCard FaceDown Nothing) cards


renderStack : UserID -> List Submission -> H.Html Msg
renderStack playerID subs =
    List.map (\{ card, orientation } -> renderCard orientation Nothing card) subs
        |> H.div [ A.class "stack" ]
        |> H.map (FlipSubmission playerID)


renderRound : UserID -> Round -> H.Html Msg
renderRound userID { submissions } =
    let
        slots =
            Dict.toList submissions
    in
    slots
        |> List.map
            (\( playerID, subs ) -> renderStack playerID (Nonempty.toList subs))
        |> H.div [ A.class "submissions" ]


renderHand : List Card -> H.Html Msg
renderHand cards =
    let
        len =
            List.length cards
    in
    H.div [ A.class "hand" ]
        (List.indexedMap (\i card -> H.map PlayCard <| renderCard FaceUp (Just <| toFloat i / toFloat (len - 1)) card) cards)


renderCard : Cards.Orientation -> Maybe Float -> Card -> H.Html Card
renderCard orientation percentage ({ color, text } as card) =
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
        , E.onClick card
        ]
        [ H.text txt ]


createOrJoinGameT : { game : Game, player : Player } -> Cmd msg
createOrJoinGameT { game, player } =
    createOrJoinGame <|
        E.object
            [ ( "game", encodeGame game )
            , ( "player", Player.encode player )
            ]


type alias Submission =
    { card : Card
    , orientation : Orientation
    }


type alias Round =
    { submissions : Dict UserID (Nonempty Submission)
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
        , ( "submissions", E.dict identity (Nonempty.toList >> E.list encodeSubmission) submissions )
        ]


encodeSubmission : Submission -> E.Value
encodeSubmission { card, orientation } =
    E.object
        [ ( "card", Cards.encode card )
        , ( "orientation", Cards.encodeOrientation orientation )
        ]


decodeSubmission : D.Decoder Submission
decodeSubmission =
    D.map2 Submission
        (D.field "card" Cards.decode)
        (D.field "orientation" Cards.decodeOrientation)


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
        (D.field "submissions" (D.dict (Utils.decodeNonempty <| decodeSubmission)))
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


uploadGameT : AppState -> Cmd msg
uploadGameT state =
    case state of
        AppState { page } ->
            case page of
                GamePage game ->
                    uploadGame (encodeGame game)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


port downloadGame : (D.Value -> msg) -> Sub msg
