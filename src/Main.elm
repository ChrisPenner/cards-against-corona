module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Cards exposing (..)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Ports exposing (..)
import Result as R



-- MAIN


main : Program () AppState Msg
main =
    Browser.document { init = init, update = updateApp, view = view, subscriptions = subscriptions }


subscriptions : AppState -> Sub Msg
subscriptions _ =
    loadCards (decodeCards >> R.map LoadCards >> DecoderResult)



-- MODEL


type ErrType
    = DecoderError D.Error


type alias Model =
    { whiteCards : List Card
    , blackCards : List Card
    }


type AppState
    = AppError ErrType
    | AppModel Model


init : flags -> ( AppState, Cmd Msg )
init _ =
    let
        model =
            AppModel
                { whiteCards = []
                , blackCards = []
                }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Empty
    | DecoderResult (Result D.Error Msg)
    | LoadCards (List Card)


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
                { whiteCards = model.whiteCards ++ white
                , blackCards = model.blackCards ++ black
                }
            , Cmd.none
            )



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
