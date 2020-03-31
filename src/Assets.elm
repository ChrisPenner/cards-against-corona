port module Assets exposing (..)

import Cards exposing (Card)
import Json.Decode as D
import List.Nonempty exposing (Nonempty)
import Utils


type alias Assets =
    { whiteCards : Nonempty Card
    , blackCards : Nonempty Card
    }



-- subscriptions : Sub Msg
-- subscriptions =
--     Sub.batch
--         [ loadedCards
--             (\r ->
--                 case decode r of
--                     Ok assets ->
--                         LoadedAssets assets
-- Err e ->
--     Failure (D.errorToString e)
-- )
-- ]


port loadedCards : (D.Value -> msg) -> Sub msg


decode : D.Value -> Result D.Error Assets
decode =
    D.decodeValue
        (D.map2 Assets
            (D.field "whiteCards" <| Utils.decodeNonempty <| Cards.decode Cards.White)
            (D.field "blackCards" <| Utils.decodeNonempty <| Cards.decode Cards.Black)
        )



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
