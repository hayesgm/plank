port module Session exposing (Session, decodeMaybeSession, decodeSession, sessionClear, sessionGuestLogin, sessionReceive)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


port sessionGuestLogin : () -> Cmd msg


port sessionReceive : (Value -> msg) -> Sub msg


port sessionClear : () -> Cmd msg


type alias Session =
    { playerId : String
    , nonce : String
    }


decodeSession : Decoder Session
decodeSession =
    Decode.map2 Session
        (Decode.field "playerId" Decode.string)
        (Decode.field "nonce" Decode.string)


decodeMaybeSession : Decoder (Maybe Session)
decodeMaybeSession =
    Decode.maybe decodeSession
