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
import Ports exposing (..)
import Random
import Result as R
import Url exposing (Url)



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
        [ loadCards (decodeCards >> R.map LoadCards >> DecoderResult)
        , joinGame (Game.decodeGame >> R.map JoinGame >> DecoderResult)
        ]



-- MODEL


type ErrType
    = DecoderError D.Error


type alias Model =
    { whiteCards : List Card
    , blackCards : List Card
    , game : Maybe Game.Game
    , navigationKey : Navigation.Key
    }


type AppState
    = AppError ErrType
    | AppModel Model


init : flags -> Url -> Navigation.Key -> ( AppState, Cmd Msg )
init _ { path } key =
    case path of
        "/" ->
            ( emptyModel key, Random.generate StartGame Game.new )

        _ ->
            ( emptyModel key, Cmd.none )


emptyModel : Navigation.Key -> AppState
emptyModel k =
    AppModel
        { whiteCards = []
        , blackCards = []
        , game = Maybe.Nothing
        , navigationKey = k
        }



-- UPDATE


type Msg
    = Empty
    | DecoderResult (Result D.Error Msg)
    | LoadCards (List Card)
    | StartGame Game.ID
    | JoinGame Game.Game


updateApp : Msg -> AppState -> ( AppState, Cmd Msg )
updateApp msg model =
    case model of
        AppModel m ->
            updateModel msg m

        e ->
            ( e, Cmd.none )


updateModel : Msg -> Model -> ( AppState, Cmd Msg )
updateModel msg model =
    case msg of
        Empty ->
            ( AppModel model, Cmd.none )

        DecoderResult (R.Err err) ->
            ( AppError (DecoderError err), Cmd.none )

        DecoderResult (R.Ok m) ->
            updateModel m model

        LoadCards cards ->
            let
                ( white, black ) =
                    List.partition (\x -> x.color == White) cards
            in
            ( AppModel
                { model
                    | whiteCards = model.whiteCards ++ white
                    , blackCards = model.blackCards ++ black
                }
            , Cmd.none
            )

        StartGame g ->
            ( AppModel model, createGame g )

        JoinGame { gameID } ->
            ( AppModel model, Navigation.pushUrl model.navigationKey ("/" ++ gameID) )



-- VIEW


view : AppState -> Browser.Document Msg
view model =
    { title = "Cards Against Corona"
    , body = renderApp model
    }


renderApp : AppState -> List (H.Html Msg)
renderApp model =
    case model of
        AppError (DecoderError err) ->
            [ H.pre [] [ H.text (D.errorToString err) ] ]

        AppModel m ->
            renderModel m


renderModel : Model -> List (H.Html Msg)
renderModel { whiteCards, blackCards } =
    [ H.h1 [] [ H.text "Cards Against Corona" ]
    , H.div
        [ A.class "cards" ]
        (renderCards (whiteCards ++ blackCards))
    ]
