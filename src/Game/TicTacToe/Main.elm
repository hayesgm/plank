module Game.TicTacToe.Main exposing (InternalMsg(..), State, game, init, msgDecoder, msgEncoder, stateDecoder, stateEncoder, subscriptions, update)

import Game exposing (GameMsg)
import Helper
import Html exposing (Html, div, text)
import Input
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


game : Game.GameServer State InternalMsg
game =
    { init = init
    , update = updateInt
    , subscriptions = subscriptions
    , msgEncoder = msgEncoder
    , msgDecoder = msgDecoder
    , stateEncoder = stateEncoder
    , stateDecoder = stateDecoder
    }


subscriptions : State -> Sub InternalMsg
subscriptions model =
    Sub.none


type alias State =
    { ticks : Int
    , pings : List Int
    }


stateEncoder : State -> Value
stateEncoder state =
    Encode.object
        [ ( "ticks", Encode.int state.ticks )
        , ( "pings", Encode.list Encode.int state.pings )
        ]


stateDecoder : Decoder State
stateDecoder =
    Decode.map2 State
        (Decode.field "ticks" Decode.int)
        (Decode.field "pings" (Decode.list Decode.int))


msgEncoder : InternalMsg -> Value
msgEncoder msg =
    case msg of
        Ping x ->
            Encode.object [ ( "ping", Encode.int x ) ]


msgDecoder : Decoder InternalMsg
msgDecoder =
    Decode.oneOf
        [ Decode.map Ping (Decode.field "ping" Decode.int)
        ]


init : ( State, Cmd InternalMsg )
init =
    ( { ticks = 0, pings = [] }, Cmd.none )


type alias Msg =
    GameMsg InternalMsg


type InternalMsg
    = Ping Int


updateInt : InternalMsg -> State -> ( State, Cmd InternalMsg )
updateInt msg model =
    case msg of
        Ping x ->
            ( { model | pings = x :: model.pings }, Cmd.none )


update : Msg -> State -> ( State, Cmd InternalMsg )
update msg model =
    case msg of
        Game.PlayerMsg pm ->
            updateInt pm model

        Game.Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )
