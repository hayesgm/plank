module Game exposing (..)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type GameName
    = TicTacToe
    | Checkers


allGames : List GameName
allGames =
    [ TicTacToe, Checkers ]


gameToString : GameName -> String
gameToString gameName =
    case gameName of
        TicTacToe ->
            "tic-tac-toe"

        Checkers ->
            "checkers"


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

                    "checkers" ->
                        Decode.succeed Checkers

                    _ ->
                        Decode.fail ("Unknown game " ++ n)
            )


gameInfoDecoder : Decoder GameInfo
gameInfoDecoder =
    Decode.map3 GameInfo
        (Decode.field "gameName" gameNameDecoder)
        (Decode.field "gameId" Decode.string)
        (Decode.field "playerId" Decode.string)
