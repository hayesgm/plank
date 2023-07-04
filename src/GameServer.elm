module GameServer exposing (GameMsgWrapper, GameServer, initGameServer)

import Game exposing (EngineMsg(..))
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)


type alias GameMsgWrapper serverMsg =
    Maybe String -> Value -> serverMsg


type alias GameServer serverMsg =
    { state : Value
    , update : Value -> Maybe String -> Value -> Result String ( Value, Value, Cmd serverMsg )
    , subscriptions : Value -> Sub serverMsg
    }


type alias GameServerData serverMsg =
    { msgWrapper : GameMsgWrapper serverMsg
    , maybeInitState : Maybe Value
    }


initGameServer : Game.Engine state msg -> GameServerData serverMsg -> ( GameServer serverMsg, Value, Cmd serverMsg )
initGameServer game { msgWrapper, maybeInitState } =
    let
        gameMsgWrapper =
            msgWrapper Nothing << game.msgEncoder

        ( initState, initCmd ) =
            case maybeInitState of
                Just st ->
                    case Decode.decodeValue game.stateDecoder st of
                        Ok currState ->
                            ( currState, Cmd.none )

                        _ ->
                            -- TODO: Maybe propagate an error here?
                            game.init

                _ ->
                    game.init

        update_ =
            \msgEnc maybePlayerId statePreEnc ->
                case ( Decode.decodeValue game.msgDecoder msgEnc, Decode.decodeValue game.stateDecoder statePreEnc ) of
                    ( Ok msg, Ok statePre ) ->
                        let
                            gameMsg =
                                case maybePlayerId of
                                    Just playerId ->
                                        PlayerMsg playerId msg

                                    Nothing ->
                                        SystemMsg msg

                            ( stateNext, cmdNext ) =
                                game.update gameMsg statePre
                        in
                        Ok ( game.stateEncoder stateNext, game.stateEncoder (game.getPublicState stateNext), Cmd.map gameMsgWrapper cmdNext )

                    ( Err msgErr, _ ) ->
                        Err ("Error decoding msg " ++ Decode.errorToString msgErr)

                    ( _, Err stateErr ) ->
                        Err ("Error decoding state " ++ Decode.errorToString stateErr)

        subscriptions_ =
            Sub.map gameMsgWrapper << game.subscriptions << Result.withDefault initState << Decode.decodeValue game.stateDecoder
    in
    ( { state = game.stateEncoder initState
      , update = update_
      , subscriptions = subscriptions_
      }
    , game.stateEncoder initState
    , Cmd.map gameMsgWrapper initCmd
    )
