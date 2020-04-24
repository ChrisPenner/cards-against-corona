port module Main exposing (..)

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
        }
    | Failure String


type alias Flags =
    { userID : String
    , assets : E.Value
    }


startGame : Maybe GameID -> Assets -> Cmd Msg
startGame gameID { whiteCards, blackCards } =
    let
        rGameID =
            case gameID of
                Nothing ->
                    newGameID

                Just gid ->
                    Random.constant gid
    in
    Random.generate StartGame
        (Random.map3
            StartGameData
            rGameID
            (shuffleCards (Nonempty.toList whiteCards))
            (shuffleCards (Nonempty.toList blackCards))
        )


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
                    , startGame Nothing decodedAssets
                    )

                pth ->
                    let
                        gameID =
                            String.dropLeft 1 pth
                    in
                    -- case newGame decodedAssets userID (String.dropLeft 1 pth) of
                    ( AppState
                        { navigationKey = key
                        , userID = userID
                        , assets = decodedAssets
                        , page = LandingPage
                        }
                    , startGame (Just gameID) decodedAssets
                    )


newPlayer : String -> Player
newPlayer playerID =
    { playerID = playerID
    , hand = []
    , submissions = []
    }



-- UPDATE


type alias StartGameData =
    { gameID : GameID, whiteDeck : List Card, blackDeck : List Card }


type Msg
    = NoMsg
    | StartGame StartGameData
    | NewGame
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

                NewGame ->
                    ( state, startGame Nothing model.assets )

                StartGame { gameID, whiteDeck, blackDeck } ->
                    case newGame whiteDeck blackDeck model.userID gameID of
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
                        |> (\s -> ( s, uploadGameFromState s ))

                Reshuffle White whiteDeck ->
                    state
                        |> mapGame (\g -> { g | whiteDeck = whiteDeck })
                        |> (\s -> ( s, uploadGameFromState s ))

                DrawWhiteCard ->
                    state
                        |> traverseGame
                            (\game ->
                                case game.whiteDeck of
                                    -- Deck is empty, reshuffle
                                    Nonempty a [] ->
                                        ( mapPlayer model.userID (addCardToHand a) game
                                        , shuffleCards (Nonempty.toList model.assets.whiteCards)
                                            |> Random.generate
                                                (\cards ->
                                                    case cards of
                                                        c :: cs ->
                                                            Reshuffle White (Nonempty c cs)

                                                        _ ->
                                                            EpicFailure "Got empty cards in shuffle"
                                                )
                                        )

                                    Nonempty a (b :: deck) ->
                                        let
                                            updatedGame =
                                                { game | whiteDeck = Nonempty b deck } |> mapPlayer model.userID (addCardToHand a)
                                        in
                                        ( updatedGame
                                        , uploadGameT updatedGame
                                        )
                            )

                NewRound ->
                    state
                        |> traverseGame
                            (\game ->
                                game
                                    |> clearCards
                                    |> (\clearedGame ->
                                            case clearedGame.blackDeck of
                                                -- Deck is empty, reshuffle
                                                Nonempty a [] ->
                                                    ( { clearedGame | blackCard = a }
                                                    , shuffleCards (Nonempty.toList model.assets.blackCards)
                                                        |> Random.generate
                                                            (\cards ->
                                                                case cards of
                                                                    c :: cs ->
                                                                        Reshuffle White (Nonempty c cs)

                                                                    _ ->
                                                                        EpicFailure "Got empty cards in shuffle"
                                                            )
                                                    )

                                                Nonempty a (b :: deck) ->
                                                    ( { clearedGame | blackCard = a, blackDeck = Nonempty b deck }
                                                    , Cmd.none
                                                    )
                                       )
                            )
                        |> (\( updatedState, cmds ) ->
                                ( updatedState, Cmd.batch [ cmds, uploadGameFromState updatedState ] )
                           )

                PlayCard card ->
                    state
                        |> mapGame (mapPlayer model.userID <| playCard card)
                        |> (\s -> ( s, uploadGameFromState s ))

                FlipSubmission playerID card ->
                    state
                        |> (mapGame << mapPlayer playerID << mapSubmissions << List.map <| flipCard card)
                        |> (\s -> ( s, uploadGameFromState s ))

                RevertSubmissions ->
                    case state of
                        AppState { userID } ->
                            state
                                |> mapGame (mapPlayer userID revertSubmissions)
                                |> (\s -> ( s, uploadGameFromState s ))

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


revertSubmissions : Player -> Player
revertSubmissions ({ submissions, hand } as player) =
    let
        subCards =
            submissions
                |> List.map (\{ card } -> card)
    in
    { player | submissions = [], hand = List.append hand subCards }


clearCards : Game -> Game
clearCards game =
    let
        collectedSubmissions =
            game.players
                |> Dict.map (\_ { submissions } -> List.map (\sub -> { sub | orientation = FaceUp }) submissions)

        clearedPlayers =
            game.players
                |> Dict.map (\_ player -> { player | submissions = [] })

        round =
            { blackCard = game.blackCard, submissions = collectedSubmissions }
    in
    { game | players = clearedPlayers, pastRounds = List.append game.pastRounds (List.singleton <| round) }


gameToRound : Game -> Round
gameToRound { blackCard, players } =
    let
        allSubmissions =
            players
                |> Dict.map (\_ { submissions } -> submissions)
    in
    { submissions = allSubmissions, blackCard = blackCard }


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


flipCard : Card -> Submission -> Submission
flipCard { text } ({ card, orientation } as submission) =
    if card.text == text then
        { submission | orientation = flipOrientation orientation }

    else
        submission


flipOrientation : Orientation -> Orientation
flipOrientation ori =
    case ori of
        FaceUp ->
            FaceDown

        FaceDown ->
            FaceUp


playCard : Card -> Player -> Player
playCard card ({ hand, submissions } as player) =
    let
        newSubmissions =
            List.append submissions (List.singleton <| { card = card, orientation = FaceDown })

        newHand =
            List.filter (\{ text } -> text /= card.text) hand
    in
    { player | hand = newHand, submissions = newSubmissions }


mapPlayer : UserID -> (Player -> Player) -> Game -> Game
mapPlayer userID mapper game =
    { game | players = Dict.update userID (Maybe.map mapper) game.players }


mapSubmissions : (List Submission -> List Submission) -> Player -> Player
mapSubmissions mapper ({ submissions } as player) =
    { player | submissions = mapper submissions }


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

                GamePage g v ->
                    H.div []
                        [ H.div [ A.class "menu" ] [ H.a [ A.class "emoji game-view-toggle", E.onClick ToggleGameView ] [ H.text "🗒️" ] ]
                        , renderGame userID v g
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
renderGame userID gameView ({ players, whiteDeck, blackDeck, blackCard, pastRounds } as game) =
    case gameView of
        Current ->
            H.div [ A.class "game" ]
                [ H.a [ A.class "white-deck", E.onClick DrawWhiteCard ] [ renderDeck (Nonempty.toList whiteDeck) ]
                , H.div [ A.class "black-card" ] [ H.map (always NoMsg) <| renderCard FaceUp Nothing blackCard ]
                , H.a [ A.class "black-deck", E.onClick NewRound ] [ renderDeck (Nonempty.toList blackDeck) ]
                , H.div [ A.class "round" ] [ renderRound userID (gameToRound game) ]
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
        |> List.filter (\( _, subs ) -> List.length subs > 0)
        |> List.map
            (\( playerID, subs ) ->
                let
                    stack =
                        renderStack playerID subs
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
            , ( "player", encodePlayer player )
            ]


type alias Game =
    { gameID : String
    , turn : UserID
    , whiteDeck : Nonempty Card
    , blackDeck : Nonempty Card
    , blackCard : Card
    , players : Dict UserID Player
    , pastRounds : List Round
    }


type alias Submission =
    { card : Card
    , orientation : Orientation
    }


type alias Player =
    { playerID : UserID
    , hand : List Card
    , submissions : List Submission
    }


type alias Round =
    { submissions : Dict UserID (List Submission)
    , blackCard : Card
    }


encodePlayer : Player -> E.Value
encodePlayer { playerID, hand, submissions } =
    E.object
        [ ( "playerID", E.string playerID )
        , ( "hand", E.list Cards.encode hand )
        , ( "submissions", E.list encodeSubmission submissions )
        ]


playerDecoder : D.Decoder Player
playerDecoder =
    D.map3 Player
        (D.field "playerID" <| D.string)
        (D.field "hand" <| D.list <| Cards.decode)
        (D.field "submissions" <| D.list <| decodeSubmission)


newGameID : Random.Generator GameID
newGameID =
    Random.map Uuid.toString Uuid.uuidGenerator



-- decodeGame


drawHand : List Card -> ( List Card, Maybe (Nonempty Card) )
drawHand startingDeck =
    case List.splitAt 5 startingDeck of
        ( hand, deck ) ->
            ( hand
            , Nonempty.fromList deck
            )


newGame : List Card -> List Card -> UserID -> GameID -> Maybe Game
newGame whiteCards blackCards userID gameID =
    case ( drawHand whiteCards, blackCards ) of
        ( ( hand, Just whiteDeck ), firstBlackCard :: (nextBlackCard :: blackDeck) ) ->
            Just
                { gameID = gameID
                , turn = userID
                , whiteDeck = whiteDeck
                , blackDeck = Nonempty nextBlackCard blackDeck
                , blackCard = firstBlackCard
                , pastRounds = []
                , players = Dict.singleton userID { playerID = userID, hand = hand, submissions = [] }
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
encodeGame { gameID, players, turn, blackDeck, whiteDeck, blackCard, pastRounds } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "turn", E.string turn )
        , ( "whiteDeck", E.list Cards.encode <| Nonempty.toList whiteDeck )
        , ( "blackDeck", E.list Cards.encode <| Nonempty.toList blackDeck )
        , ( "blackCard", Cards.encode blackCard )
        , ( "players", E.dict identity encodePlayer players )
        , ( "pastRounds", E.list encodeRound pastRounds )
        ]


encodeRound : Round -> E.Value
encodeRound { submissions, blackCard } =
    E.object
        [ ( "blackCard", Cards.encode blackCard )
        , ( "submissions", E.dict identity (E.list encodeSubmission) submissions )
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
        (D.field "turn" D.string)
        (D.field "whiteDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "blackDeck" <| Utils.decodeNonempty <| Cards.decode)
        (D.field "blackCard" Cards.decode)
        (D.field "players" (D.dict playerDecoder))
        (D.field "pastRounds" <| D.list roundDecoder)


roundDecoder : D.Decoder Round
roundDecoder =
    D.map2 Round
        (D.field "submissions" (D.dict (D.list <| decodeSubmission)))
        (D.field "blackCard" Cards.decode)


shuffleCards : List Card -> Random.Generator (List Card)
shuffleCards cards =
    cards |> Random.List.shuffle


addCardToHand : Card -> Player -> Player
addCardToHand c player =
    { player | hand = List.append player.hand [ c ] }



-- Outgoing


port createOrJoinGame : E.Value -> Cmd msg


port uploadGame : E.Value -> Cmd msg


uploadGameFromState : AppState -> Cmd msg
uploadGameFromState state =
    case state of
        AppState { page } ->
            case page of
                GamePage game _ ->
                    uploadGameT game

                _ ->
                    Cmd.none

        _ ->
            Cmd.none


uploadGameT : Game -> Cmd msg
uploadGameT game =
    uploadGame (encodeGame game)


port downloadGame : (D.Value -> msg) -> Sub msg
