module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Navigation as Navigation
import Cards exposing (..)
import Debug exposing (..)
import Game
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Loading exposing (..)
import Ports exposing (..)
import Random
import Result as R
import Url exposing (Url)
import Utils exposing (..)



-- MAIN


main : Program () AppState Msg
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
        [ Sub.map GameMsg Game.subscriptions
        , Game.joinGame JoinGame
        ]



-- MODEL


type ErrType
    = DecoderError D.Error


type Page
    = LandingPage
    | GamePage Game.Game


type alias AppState =
    { globals : Globals
    , page : Page
    }


type alias Globals =
    { navigationKey : Navigation.Key }


init : flags -> Url -> Navigation.Key -> ( AppState, Cmd Msg )
init _ { path } key =
    case path of
        "/" ->
            ( { globals = { navigationKey = key }, page = LandingPage }, Random.generate StartGame Game.newGameID )

        pth ->
            ( { globals = { navigationKey = key }, page = LandingPage }, Game.createGame (String.dropLeft 1 pth) )



-- UPDATE


type Msg
    = Empty
    | StartGame Game.ID
    | JoinGame Game.ID
    | GameMsg Game.Msg


updateApp : Msg -> AppState -> ( AppState, Cmd Msg )
updateApp msg ({ page, globals } as model) =
    (case ( page, msg ) of
        ( _, Empty ) ->
            ( page, Cmd.none )

        ( pg, StartGame id ) ->
            ( pg, Game.createGame id )

        ( _, JoinGame gameID ) ->
            case Game.load gameID of
                ( g, cmds ) ->
                    ( GamePage g, Cmd.batch [ cmds, Navigation.pushUrl globals.navigationKey ("/" ++ gameID) ] )

        ( LandingPage, _ ) ->
            ( page, Cmd.none )

        ( GamePage g, GameMsg m ) ->
            case Game.update m g of
                ( newGame, cmd ) ->
                    ( GamePage newGame, cmd )
    )
        |> Tuple.mapFirst (\p -> { model | page = p })



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
    , body = renderApp model
    }


renderApp : AppState -> List (H.Html Msg)
renderApp { page } =
    case page of
        LandingPage ->
            renderLandingPage

        GamePage g ->
            Game.render g


renderLandingPage : List (H.Html msg)
renderLandingPage =
    [ H.h1 [] [ H.text "Cards Against Corona" ]
    , H.text "Starting New Game..."
    ]
