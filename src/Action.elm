port module Action exposing (..)

import Json.Encode exposing (Value)


port sendAction : Value -> Cmd msg


port receiveAction : (( Maybe String, Value ) -> msg) -> Sub msg


port giveState : Value -> Cmd msg


port receiveState : (Value -> msg) -> Sub msg


port joinGame : ( String, String ) -> Cmd msg


port gameConnected : (Value -> msg) -> Sub msg


port newGame : ( String, String ) -> Cmd msg


port newGameResp : (Value -> msg) -> Sub msg


port loadCss : String -> Cmd msg


port disconnect : () -> Cmd msg
