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
    | GamePage Game GameView


type GameView
    = Current
    | Past


type AppState
    = AppState
        { userID : String
        , navigationKey : Navigation.Key
        , assets : Assets
        , page : Page
        , theme : String
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
                        , theme = "fruity"
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
                                , theme = "fruity"
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
    | DrawWhiteCard
    | NewRound
    | Reshuffle Color (Nonempty Card)
    | PlayCard Card
    | FlipSubmission UserID Card
    | RevertSubmissions
    | ToggleGameView


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
                    ( AppState { model | page = GamePage game Current }
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

                DrawWhiteCard ->
                    state
                        |> traverseGame
                            (\game ->
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
                            )

                NewRound ->
                    state
                        |> mapGame (\g -> { g | pastRounds = List.append g.pastRounds (List.singleton <| pastifyRound <| g.round) })
                        |> traverseGame
                            (\game ->
                                case game.blackDeck of
                                    -- Deck is empty, reshuffle
                                    Nonempty a [] ->
                                        ( game
                                            |> mapRound (always { blackCard = a, submissions = Dict.empty })
                                        , shuffleCards model.assets.blackCards (Reshuffle Black)
                                        )

                                    Nonempty a (b :: deck) ->
                                        ( game |> mapRound (\r -> { r | blackCard = a }) |> (\g -> { g | blackDeck = Nonempty b deck })
                                        , Cmd.none
                                        )
                            )
                        |> Tuple.mapFirst (mapGame << mapRound << mapSubmissions <| always Dict.empty)
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

                RevertSubmissions ->
                    case state of
                        AppState { userID } ->
                            state
                                |> mapGame
                                    (\({ round, players } as game) ->
                                        case Dict.get userID round.submissions of
                                            Nothing ->
                                                game

                                            Just subs ->
                                                { game
                                                    | round = { round | submissions = Dict.remove userID round.submissions }
                                                    , players = Dict.update userID (Maybe.map (\p -> { p | hand = List.append p.hand (subsToCards subs) })) <| game.players
                                                }
                                    )
                                |> (\s -> ( s, uploadGameT s ))

                        _ ->
                            ( state, Cmd.none )

                ToggleGameView ->
                    case model.page of
                        GamePage g v ->
                            case v of
                                Current ->
                                    ( AppState { model | page = GamePage g Past }, Cmd.none )

                                Past ->
                                    ( AppState { model | page = GamePage g Current }, Cmd.none )

                        _ ->
                            ( state, Cmd.none )


pastifyRound : Round -> Round
pastifyRound ({ submissions } as round) =
    { round | submissions = Dict.map (\_ subs -> Nonempty.map (\sub -> { sub | orientation = FaceUp }) subs) submissions }


subsToCards : Nonempty Submission -> List Card
subsToCards =
    Nonempty.map (\{ card } -> card)
        >> Nonempty.toList


mapGame : (Game -> Game) -> AppState -> AppState
mapGame mapper state =
    case state of
        AppState ({ page } as model) ->
            case page of
                GamePage g v ->
                    AppState { model | page = GamePage (mapper g) v }

                _ ->
                    state

        _ ->
            state


traverseGame : (Game -> ( Game, Cmd a )) -> AppState -> ( AppState, Cmd a )
traverseGame mapper state =
    case state of
        AppState ({ page } as model) ->
            case page of
                GamePage g v ->
                    case mapper g of
                        ( newG, cmds ) ->
                            ( AppState { model | page = GamePage newG v }, cmds )

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

        AppState { page, userID, theme } ->
            H.div
                [ A.class theme
                ]
                [ case page of
                    LandingPage ->
                        renderLandingPage

                    GamePage g v ->
                        H.div []
                            [ H.div [ A.class "menu" ] [ H.a [ A.class "emoji game-view-toggle", E.onClick ToggleGameView ] [ H.text "🗒️" ] ]
                            , renderGame userID v g
                            ]
                ]


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


renderGame : UserID -> GameView -> Game -> H.Html Msg
renderGame userID gameView ({ players, whiteDeck, blackDeck, round, pastRounds } as game) =
    case gameView of
        Current ->
            H.div [ A.class "game" ]
                [ H.a [ A.class "white-deck", E.onClick DrawWhiteCard ] [ renderDeck (Nonempty.toList whiteDeck) ]
                , H.div [ A.class "black-card" ] [ H.map (always NoMsg) <| renderCard FaceUp Nothing round.blackCard ]
                , H.a [ A.class "black-deck", E.onClick NewRound ] [ renderDeck (Nonempty.toList blackDeck) ]
                , H.div [ A.class "round" ] [ renderRound userID round ]
                , case Dict.get userID players of
                    Nothing ->
                        H.div [ A.class "hand" ] []

                    Just { hand } ->
                        renderHand hand
                , renderStats game
                ]

        Past ->
            H.div []
                [ H.h1 [] [ H.text "Past Rounds" ]
                , renderPastRounds userID pastRounds
                ]


renderDeck : List Card -> H.Html Msg
renderDeck cards =
    H.div [ A.class "deck" ] <|
        List.map (H.map (always NoMsg) << renderCard FaceDown Nothing) (List.take 10 cards)


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
            (\( playerID, subs ) ->
                let
                    stack =
                        renderStack playerID (Nonempty.toList subs)
                in
                if playerID == userID then
                    H.div [ A.class "submission your-submission" ]
                        [ H.div [ A.class "pointer emoji" ] [ H.text "🙋\u{200D}♂️" ]
                        , stack
                        , H.div [ A.class "undo emoji", E.onClick RevertSubmissions ] [ H.text "↩️" ]
                        ]

                else
                    H.div [ A.class "submission" ] [ stack ]
            )
        |> H.div [ A.class "submissions" ]


renderPastRounds : UserID -> List Round -> H.Html Msg
renderPastRounds userID =
    List.map
        (\({ submissions, blackCard } as round) ->
            H.div [ A.class "past-round" ]
                [ H.map (always NoMsg) <| renderCard FaceUp Nothing blackCard
                , renderRound userID round
                ]
        )
        >> H.div [ A.class "past-rounds" ]
        -- Disable clicks from past rounds
        >> H.map (always NoMsg)


renderHand : List Card -> H.Html Msg
renderHand cards =
    let
        len =
            List.length cards
    in
    H.div [ A.class "hand" ]
        (List.indexedMap (\i card -> H.map PlayCard <| renderCard FaceUp (Just <| toFloat i / toFloat (len - 1)) card) cards)


renderStats : Game -> H.Html Msg
renderStats { players, pastRounds } =
    H.div [ A.class "stats" ]
        [ H.div [] [ H.text ((String.fromInt <| Dict.size players) ++ " Player(s) in the game") ]
        , H.div [] [ H.text ("You're on round " ++ (String.fromInt <| List.length pastRounds + 1)) ]
        ]


renderCard : Cards.Orientation -> Maybe Float -> Card -> H.Html Card
renderCard orientation percentage ({ color, text } as card) =
    let
        txt =
            case orientation of
                FaceUp ->
                    text

                FaceDown ->
                    "Cards Against Coronavirus"

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
    , pastRounds : List Round
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
                , pastRounds = []
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
encodeGame { gameID, players, turn, blackDeck, whiteDeck, round, pastRounds } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        , ( "blackDeck", E.list Cards.encode <| Nonempty.toList blackDeck )
        , ( "whiteDeck", E.list Cards.encode <| Nonempty.toList whiteDeck )
        , ( "round", encodeRound round )
        , ( "pastRounds", E.list encodeRound pastRounds )
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
    D.map7 Game
        (D.field "gameID" D.string)
        (D.field "players" (D.dict Player.decode))
        (D.field "turn" D.string)
        (D.field "whiteDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "blackDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "round" roundDecoder)
        (D.field "pastRounds" <| D.list roundDecoder)


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
                GamePage game _ ->
                    uploadGame (encodeGame game)

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


port downloadGame : (D.Value -> msg) -> Sub msg
