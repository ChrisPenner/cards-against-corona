module Main exposing (..)

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
import Game exposing (Game)
import Html as H exposing (Html)
import Json.Decode as D
import Loading exposing (..)
import Player exposing (Player)
import Ports exposing (..)
import Random
import Result as R
import Result.Extra as R
import Url exposing (Url)
import Utils exposing (..)



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
            todo ""

        Browser.External _ ->
            todo ""


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
        [ Sub.map AssetMsg Assets.subscriptions
        , Game.joinedGame (Game.decode >> R.unpack (D.errorToString >> EpicFailure) JoinGame)
        ]



-- MODEL


type ErrType
    = DecoderError D.Error


type Page
    = LandingPage
    | GamePage Game.Game
    | FailurePage String


type alias AppState =
    { user : Player
    , navigationKey : Navigation.Key
    , assets : Assets
    , page : Page
    }


type alias Flags =
    { user : Player
    , assets : Assets
    }


init : Flags -> Url -> Navigation.Key -> ( AppState, Cmd Msg )
init { user } { path } key =
    case path of
        "/" ->
            ( { navigationKey = key
              , user = user
              , assets = Loading
              , page = LandingPage
              }
            , Random.generate identity (Random.map2 StartGame Game.newGameID Random.independentSeed)
            )

        pth ->
            ( { navigationKey = key
              , user = user
              , assets = Loading
              , page = LandingPage
              }
            , Game.createOrJoinGameT (Game.new user (String.dropLeft 1 pth))
            )



-- UPDATE


type Msg
    = Empty
    | StartGame Game.ID Random.Seed
    | JoinGame Game
    | AssetMsg Assets.Msg
    | EpicFailure String


updateApp : Msg -> AppState -> ( AppState, Cmd Msg )
updateApp msg model =
    case msg of
        Empty ->
            ( model, Cmd.none )

        AssetMsg (Assets.Failure err) ->
            ( { model | page = FailurePage err }, Cmd.none )

        AssetMsg (Assets.LoadedCards cards) ->
            let
                ( white, black ) =
                    List.partition (\x -> x.color == White) cards
            in
            ( { model | assets = Loaded { whiteCards = white, blackCards = black } }, Cmd.none )

        EpicFailure err ->
            ( { model | page = FailurePage err }, Cmd.none )

        -- ( FailurePage err, _ ) ->
        --     ( FailurePage err, Cmd.none )
        StartGame gameID ->
            ( model
            , Game.createOrJoinGameT (Game.new model.user gameID)
            )

        JoinGame game ->
            case Game.load game of
                ( g, cmds ) ->
                    ( { model | page = GamePage g }, Cmd.batch [ cmds, Navigation.pushUrl model.navigationKey ("/" ++ game.gameID) ] )



-- updateModel : Msg -> AppState -> ( AppState, Cmd Msg )
-- updateModel msg model =
--     case msg of
--         Empty ->
--             ( model, Cmd.none )
--         StartGame g ->
--             ( model, Game.createGame g )
--         JoinGame { gameID } ->
--             ( model, Navigation.pushUrl model.globals.navigationKey ("/" ++ gameID) )
--         GameMsg m ->
--             ( {model | , Navigation.pushUrl model.globals.navigationKey ("/" ++ gameID) )
-- VIEW


view : AppState -> Browser.Document Msg
view model =
    { title = "Cards Against Corona"
    , body = [ renderApp model ]
    }


renderApp : AppState -> H.Html Msg
renderApp { assets, page, user } =
    case page of
        LandingPage ->
            renderLandingPage

        GamePage g ->
            case assets of
                Loading ->
                    renderLoadingPage

                Loaded loadedAssets ->
                    Game.render user loadedAssets g

        FailurePage err ->
            renderFailurePage err


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
