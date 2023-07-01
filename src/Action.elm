port module Action exposing (giveState, receiveAction)

import Json.Encode exposing (Value)


port receiveAction : (Value -> msg) -> Sub msg


port giveState : Value -> Cmd msg
