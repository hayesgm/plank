module Game exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type GameName
    = TicTacToe


allGames : List GameName
allGames =
    [ TicTacToe ]


gameToString : GameName -> String
gameToString gameName =
    case gameName of
        TicTacToe ->
            "tic-tac-toe"


gameReflect : GameName -> String
gameReflect gameName =
    case gameName of
        TicTacToe ->
            "TicTacToe"


gameFromString : String -> Maybe GameName
gameFromString gameName =
    case gameName of
        "tic-tac-toe" ->
            Just TicTacToe

        _ ->
            Nothing


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
    , update : InboundMsg engineMsg gameMsg -> model -> ( model, Cmd (GameMsg engineMsg gameMsg) )
    , view : AssetMapping -> model -> Html (GameMsg engineMsg gameMsg)
    , css : AssetMapping -> Maybe String
    , subscriptions : model -> Sub (GameMsg engineMsg gameMsg)
    , gameMsgEncoder : gameMsg -> Value
    , gameMsgDecoder : Decoder gameMsg
    , engineMsgEncoder : engineMsg -> Value
    , engineMsgDecoder : Decoder engineMsg
    , modelEncoder : model -> Value
    , modelDecoder : Decoder model
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    , setGameState : state -> model -> model
    }


type alias Engine state msg =
    { init : ( state, Cmd msg )
    , update : EngineMsg msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    , publicStateEncoder : state -> Value
    , publicStateDecoder : Decoder state
    }


type alias GameInfo =
    { gameName : GameName
    , gameId : String
    , gameState : Value
    , playerId : String
    }


gameNameDecoder : Decoder GameName
gameNameDecoder =
    Decode.string
        |> Decode.andThen
            (\n ->
                case gameFromString n of
                    Just g ->
                        Decode.succeed g

                    _ ->
                        Decode.fail ("Unknown game " ++ n)
            )


gameInfoDecoder : Decoder GameInfo
gameInfoDecoder =
    Decode.map4 GameInfo
        (Decode.field "gameName" gameNameDecoder)
        (Decode.field "gameId" Decode.string)
        (Decode.field "gameState" Decode.value)
        (Decode.field "playerId" Decode.string)


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
