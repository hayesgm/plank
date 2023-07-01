port module Action exposing (gameConnected, giveState, joinGame, newGame, newGameResp, receiveAction, receiveState, sendAction)

import Json.Encode exposing (Value)


port sendAction : Value -> Cmd msg


port receiveAction : (Value -> msg) -> Sub msg


port giveState : Value -> Cmd msg


port receiveState : (Value -> msg) -> Sub msg


port joinGame : String -> Cmd msg


port gameConnected : (Value -> msg) -> Sub msg


port newGame : String -> Cmd msg


port newGameResp : (Value -> msg) -> Sub msg
