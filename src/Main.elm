module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Debug exposing (log)
import Html as H
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode as E



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    ()


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            ()
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Empty ->
            ( model, Cmd.none )



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
