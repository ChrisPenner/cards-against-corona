module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Cards exposing (..)
import Debug exposing (log)
import Html as H
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Ports exposing (..)
import Result as R



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadCards (decodeCards >> R.map LoadCards >> DecoderResult)



-- MODEL


type ErrType
    = DecoderError D.Error


type Model
    = Error ErrType
    | Model { whiteCards : List Card, blackCards : List Card }


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Empty ->
            ( model, Cmd.none )

        DecoderResult (R.Err err) ->
            ( Error (DecoderError err), Cmd.none )

        DecoderResult (R.Ok m) ->
            update m model

        LoadCards cards ->
            let
                ( white, black ) =
                    List.partition (\x -> x.color == White) cards
            in
            ( Model { whiteCards = white, blackCards = black }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello world"
    , body = render model
    }


render : Model -> List (H.Html Msg)
render _ =
    [ H.h1 [] [ H.text "Cards Against Corona" ]
    ]
