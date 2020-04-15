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
        Browser.Internal x ->
            log (Url.toString x) Empty

        Browser.External x ->
            log x Empty


onUrlChange : Url.Url -> Msg
onUrlChange { path } =
    case path of
        "/" ->
            Empty

        _ ->
            Empty


subscriptions : AppState -> Sub Msg
subscriptions _ =
    Sub.batch
        [ joinedGame (decodeGame >> R.unpack (D.errorToString >> EpicFailure) JoinGame)
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
                            , createOrJoinGameT g
                            )

                        Nothing ->
                            ( Failure "Not enough cards in deck to start a game", Cmd.none )



-- UPDATE


type Msg
    = Empty
    | StartGame GameID
    | JoinGame Game
    | EpicFailure String
    | DrawCard Color
    | Reshuffle Color (Nonempty Card)


updateApp : Msg -> AppState -> ( AppState, Cmd Msg )
updateApp msg state =
    case state of
        Failure err ->
            ( Failure err, Cmd.none )

        AppState model ->
            case msg of
                Empty ->
                    ( AppState model, Cmd.none )

                EpicFailure err ->
                    ( Failure err, Cmd.none )

                StartGame gameID ->
                    case newGame model.assets model.userID gameID of
                        Just g ->
                            ( AppState model
                            , createOrJoinGameT g
                            )

                        Nothing ->
                            ( Failure "Not enough cards in deck to start a game", Cmd.none )

                JoinGame game ->
                    case loadGame game of
                        ( g, cmds ) ->
                            ( AppState { model | page = GamePage g }
                            , Cmd.batch [ cmds, Navigation.pushUrl model.navigationKey ("/" ++ game.gameID) ]
                            )

                Reshuffle Black blackDeck ->
                    case model.page of
                        GamePage g ->
                            ( AppState { model | page = GamePage { g | blackDeck = blackDeck } }
                            , Cmd.none
                            )

                        _ ->
                            ( Failure "Can't shuffle on landing page", Cmd.none )

                Reshuffle White whiteDeck ->
                    case model.page of
                        GamePage g ->
                            ( AppState { model | page = GamePage { g | whiteDeck = whiteDeck } }
                            , Cmd.none
                            )

                        _ ->
                            ( Failure "Can't shuffle on landing page", Cmd.none )

                DrawCard col ->
                    case ( col, model.page ) of
                        ( White, GamePage ({ whiteDeck } as game) ) ->
                            case whiteDeck of
                                -- Deck is empty, reshuffle
                                Nonempty a [] ->
                                    ( AppState { model | page = GamePage (mapPlayer model.userID (addCardToHand a) game) }
                                    , shuffleCards model.assets.whiteCards (Reshuffle White)
                                    )

                                Nonempty a (b :: deck) ->
                                    ( AppState
                                        { model
                                            | page =
                                                GamePage
                                                    ({ game | whiteDeck = Nonempty b deck }
                                                        |> mapPlayer model.userID (addCardToHand a)
                                                    )
                                        }
                                    , Cmd.none
                                    )

                        ( Black, GamePage ({ blackDeck } as game) ) ->
                            case blackDeck of
                                -- Deck is empty, reshuffle
                                Nonempty a [] ->
                                    ( AppState { model | page = GamePage { game | blackCard = Just a } }
                                    , shuffleCards model.assets.blackCards (Reshuffle Black)
                                    )

                                Nonempty a (b :: deck) ->
                                    ( AppState { model | page = GamePage { game | blackDeck = Nonempty b deck, blackCard = Just a } }
                                    , Cmd.none
                                    )

                        _ ->
                            ( Failure "Got unexpected message on Landing page", Cmd.none )


mapPlayer : UserID -> (Player -> Player) -> Game -> Game
mapPlayer userID mapper game =
    { game | players = Dict.update userID (Maybe.map mapper) game.players }


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
renderGame userID { players, turn, whiteDeck, blackDeck, blackCard } =
    H.div [ A.class "game" ]
        [ H.h1 []
            [ H.text "Cards Against Corona" ]
        , H.div [ A.class "container" ]
            [ H.a [ E.onClick (DrawCard White) ] [ renderDeck (Nonempty.toList whiteDeck) ]
            , H.div []
                [ case blackCard of
                    Nothing ->
                        H.text "draw a black card"

                    Just card ->
                        Cards.renderCard Nothing card
                ]
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
                    , renderCards hand
                    ]
        ]


renderDeck : List Card -> H.Html msg
renderDeck cards =
    H.div [ A.class "deck" ] <|
        List.map
            (\{ color } ->
                Cards.renderCard Nothing { text = "Cards Against Humanity", color = color }
            )
            cards


createOrJoinGameT : Game -> Cmd msg
createOrJoinGameT { gameID, players, turn, blackCard, blackDeck, whiteDeck } =
    E.object
        [ ( "gameID", E.string gameID )
        , ( "players", E.dict identity Player.encode players )
        , ( "turn", E.string turn )
        , ( "blackCard", Maybe.withDefault E.null <| Maybe.map Cards.encode blackCard )
        , ( "blackDeck", E.list Cards.encode <| Nonempty.toList blackDeck )
        , ( "whiteDeck", E.list Cards.encode <| Nonempty.toList whiteDeck )
        ]
        |> createOrJoinGame


type alias Game =
    { gameID : String
    , players : Dict UserID Player
    , turn : Player.ID
    , blackCard : Maybe Card
    , whiteDeck : Nonempty Card
    , blackDeck : Nonempty Card
    }


newGameID : Random.Generator GameID
newGameID =
    Random.map Uuid.toString Uuid.uuidGenerator



-- decodeGame


loadGame : Game -> ( Game, Cmd msg )
loadGame game =
    ( game, Cmd.none )


drawHand : Nonempty Card -> ( Maybe (Nonempty Card), Maybe (Nonempty Card) )
drawHand startingDeck =
    case List.splitAt 5 (Nonempty.toList startingDeck) of
        ( hand, deck ) ->
            ( Nonempty.fromList hand
            , Nonempty.fromList deck
            )


newGame : Assets -> UserID -> GameID -> Maybe Game
newGame { whiteCards, blackCards } userID gameID =
    case drawHand whiteCards of
        ( Just hand, Just deck ) ->
            Just
                { gameID = gameID
                , players = Dict.singleton userID { playerID = userID, hand = hand }
                , turn = userID
                , blackCard = Nothing
                , whiteDeck = whiteCards
                , blackDeck = blackCards
                }

        _ ->
            Nothing


newRound : Assets -> Cmd Msg
newRound {} =
    Cmd.none


decodeGame : D.Value -> Result D.Error Game
decodeGame =
    D.decodeValue gameDecoder


gameDecoder : D.Decoder Game
gameDecoder =
    D.map6 Game
        (D.field "gameID" D.string)
        (D.field "players" (D.dict Player.decode))
        (D.field "turn" D.string)
        (D.field "blackCard" <| D.maybe (Cards.decode Black))
        (D.field "whiteDeck" <| Utils.decodeNonempty <| Cards.decode White)
        (D.field "blackDeck" <| Utils.decodeNonempty <| Cards.decode Black)


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
    { player | hand = Nonempty.append player.hand (Nonempty c []) }



-- Outgoing


port createOrJoinGame : E.Value -> Cmd msg



-- Incoming


port joinedGame : (D.Value -> msg) -> Sub msg
