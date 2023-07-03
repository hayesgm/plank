module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias PlayerId =
    String


type EngineMsg msg
    = PlayerMsg PlayerId msg
    | SystemMsg msg


type GameMsg engineMsg gameMsg
    = ForEngine engineMsg
    | ForSelf gameMsg


type InboundMsg engineMsg gameMsg
    = EngineMsg (EngineMsg engineMsg)
    | GameMsg gameMsg


type alias Game model state engineMsg gameMsg =
    { init : PlayerId -> state -> ( model, Maybe (GameMsg engineMsg gameMsg), Cmd (GameMsg engineMsg gameMsg) )
    , update : gameMsg -> model -> state -> ( model, Cmd (GameMsg engineMsg gameMsg) )
    , view : AssetMapping -> model -> state -> Html (GameMsg engineMsg gameMsg)
    , css : AssetMapping -> Maybe String
    , subscriptions : model -> state -> Sub (GameMsg engineMsg gameMsg)
    , gameMsgEncoder : gameMsg -> Value
    , gameMsgDecoder : Decoder gameMsg
    , modelEncoder : model -> Value
    , modelDecoder : Decoder model
    , engine : Engine state engineMsg
    }


type alias Engine state msg =
    { init : ( state, Cmd msg )
    , update : EngineMsg msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    , getPublicState : state -> state
    }


type alias AssetMapping =
    String -> Maybe String


decodeAssetMapping : Decoder (String -> AssetMapping)
decodeAssetMapping =
    Decode.dict Decode.string
        |> Decode.map
            (\dict ->
                \pkg asset ->
                    Dict.get ("./src/Game/" ++ pkg ++ "/assets/" ++ asset) dict
            )


encodePlayerId : PlayerId -> Value
encodePlayerId =
    Encode.string


decodePlayerId : Decoder PlayerId
decodePlayerId =
    Decode.string
