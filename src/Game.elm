module Game exposing (..)

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


type GameMsg msg
    = PlayerMsg msg
    | Tick


type alias Game model state msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , view : model -> Html msg
    , subscriptions : model -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , modelEncoder : model -> Value
    , modelDecoder : Decoder model
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    , setGameState : state -> model -> model
    }


type alias Engine state msg =
    { init : ( state, Cmd msg )
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , msgEncoder : msg -> Value
    , msgDecoder : Decoder msg
    , stateEncoder : state -> Value
    , stateDecoder : Decoder state
    }


type alias GameInfo =
    { gameName : GameName
    , gameId : String
    , playerId : String
    }


gameNameDecoder : Decoder GameName
gameNameDecoder =
    Decode.string
        |> Decode.andThen
            (\n ->
                case n of
                    "tic-tac-toe" ->
                        Decode.succeed TicTacToe

                    _ ->
                        Decode.fail ("Unknown game " ++ n)
            )


gameInfoDecoder : Decoder GameInfo
gameInfoDecoder =
    Decode.map3 GameInfo
        (Decode.field "gameName" gameNameDecoder)
        (Decode.field "gameId" Decode.string)
        (Decode.field "playerId" Decode.string)
