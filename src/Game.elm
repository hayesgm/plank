module Game exposing (..)

import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)


type GameMsg msg
    = PlayerMsg msg
    | Tick


type alias Game state msg =
    { init : ( state, Cmd msg )
    , update : msg -> state -> ( state, Cmd msg )
    , view : state -> Html msg
    , subscriptions : state -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    }


type alias GameServer state msg =
    { init : ( state, Cmd msg )
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    }
