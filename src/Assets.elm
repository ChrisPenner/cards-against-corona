port module Assets exposing (..)

import Cards exposing (Card)
import Json.Decode as D


type alias Assets =
    { whiteCards : List Card
    , blackCards : List Card
    }


type Msg
    = LoadedCards (List Card)
    | Failure String


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ loadedCards
            (\r ->
                case Cards.decode r of
                    Ok cards ->
                        LoadedCards cards

                    Err e ->
                        Failure (D.errorToString e)
            )
        ]


port loadedCards : (D.Value -> msg) -> Sub msg



-- LoadedCards cards ->
--     let
--         ( white, black ) =
--             List.partition (\x -> x.color == White) cards
--     in
--     ( { g
--         | whiteCards = Loaded (Loading.withDefault [] g.whiteCards ++ white)
--         , blackCards = Loaded (Loading.withDefault [] g.blackCards ++ black)
--       }
--     , Cmd.none
--     )
