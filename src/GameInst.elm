module GameInst exposing (GameInst, GameInstData, MsgWrapper, initGameInst)

import Action
import Game exposing (EngineMsg(..))
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias MsgWrapper msg =
    Bool -> Value -> msg


type alias GameInst msg =
    { model : Value
    , state : Value
    , update : Bool -> Value -> Value -> Value -> Result String ( Maybe Value, Maybe Value, Cmd msg )
    , view : Value -> Value -> Html msg
    , subscriptions : Value -> Value -> Sub msg
    , gameId : String
    , playerId : String
    }


type alias GameInstData msg =
    { msgWrapper : MsgWrapper msg
    , gameName : String
    , gameId : String
    , gameState : Value
    , playerId : String
    , assetMapping : String -> Game.AssetMapping
    }


initGameInst : Game.Game model state engineMsg gameMsg -> GameInstData msg -> Result String ( GameInst msg, Maybe msg, Cmd msg )
initGameInst game { msgWrapper, gameName, gameId, gameState, playerId, assetMapping } =
    case Decode.decodeValue game.engine.stateDecoder gameState of
        Err err ->
            Err (Decode.errorToString err)

        Ok initState ->
            let
                ( initModel, maybeMsg, initCmd ) =
                    game.init playerId initState

                inboundMsgDecoder : Decoder (Game.InboundMsg engineMsg gameMsg)
                inboundMsgDecoder =
                    Decode.oneOf
                        [ Decode.map Game.EngineMsg
                            (Decode.map2
                                Game.PlayerMsg
                                (Decode.field "PID" Decode.string)
                                (Decode.field "PM" game.engine.msgDecoder)
                            )
                        , Decode.map Game.EngineMsg (Decode.map Game.SystemMsg (Decode.field "SM" game.engine.msgDecoder))
                        , Decode.map Game.GameMsg (Decode.field "GM" game.gameMsgDecoder)
                        ]

                gameMsgEncoder : Game.GameMsg engineMsg gameMsg -> Value
                gameMsgEncoder gameMsg =
                    case gameMsg of
                        Game.ForEngine engineMsg ->
                            Encode.object
                                [ ( "PID", Encode.string playerId )
                                , ( "PM", game.engine.msgEncoder engineMsg )
                                ]

                        Game.ForSelf gameMsg_ ->
                            Encode.object
                                [ ( "GM", game.gameMsgEncoder gameMsg_ ) ]

                gameMsgWrapper =
                    msgWrapper False << gameMsgEncoder

                update_ =
                    \sendMsg msgEnc modelPreEnc stateEnc ->
                        case ( Decode.decodeValue inboundMsgDecoder msgEnc, Decode.decodeValue game.modelDecoder modelPreEnc, Decode.decodeValue game.engine.stateDecoder stateEnc ) of
                            ( Ok (Game.GameMsg gameMsg), Ok modelPre, Ok state ) ->
                                let
                                    ( modelNext, cmdNext ) =
                                        game.update gameMsg modelPre state
                                in
                                Ok ( Just (game.modelEncoder modelNext), Nothing, Cmd.map gameMsgWrapper cmdNext )

                            ( Ok (Game.EngineMsg engineMsg), Ok modelPre, Ok state ) ->
                                let
                                    ( stateNext, gameCmd ) =
                                        game.engine.update engineMsg state

                                    sendActionCmd =
                                        case ( sendMsg, engineMsg ) of
                                            ( True, Game.PlayerMsg playerId_ playerMsg ) ->
                                                if playerId == playerId_ then
                                                    Action.sendAction (game.engine.msgEncoder playerMsg)

                                                else
                                                    Cmd.none

                                            _ ->
                                                Cmd.none
                                in
                                Ok ( Nothing, Just (game.engine.stateEncoder stateNext), Cmd.batch [ Cmd.map gameMsgWrapper (Cmd.map Game.ForEngine gameCmd), sendActionCmd ] )

                            ( Err msgErr, _, _ ) ->
                                Err ("Error decoding msg " ++ Decode.errorToString msgErr)

                            ( _, Err modelErr, _ ) ->
                                Err ("Error decoding model " ++ Decode.errorToString modelErr)

                            ( _, _, Err stateErr ) ->
                                Err ("Error decoding state " ++ Decode.errorToString stateErr)

                gameAssetMapping =
                    assetMapping gameName

                view_ =
                    \modelEnc stateEnc ->
                        let
                            model =
                                modelEnc
                                    |> Decode.decodeValue game.modelDecoder
                                    |> Result.withDefault initModel

                            state =
                                stateEnc
                                    |> Decode.decodeValue game.engine.stateDecoder
                                    |> Result.withDefault initState
                        in
                        Html.map gameMsgWrapper (game.view gameAssetMapping model state)

                subscriptions_ =
                    \modelEnc stateEnc ->
                        let
                            model =
                                modelEnc
                                    |> Decode.decodeValue game.modelDecoder
                                    |> Result.withDefault initModel

                            state =
                                stateEnc
                                    |> Decode.decodeValue game.engine.stateDecoder
                                    |> Result.withDefault initState
                        in
                        Sub.map gameMsgWrapper (game.subscriptions model state)

                loadCss =
                    case game.css gameAssetMapping of
                        Just asset ->
                            Action.loadCss asset

                        _ ->
                            Cmd.none
            in
            Ok
                ( { model = game.modelEncoder initModel
                  , state = game.engine.stateEncoder initState
                  , update = update_
                  , view = view_
                  , subscriptions = subscriptions_
                  , gameId = gameId
                  , playerId = playerId
                  }
                , Maybe.map gameMsgWrapper maybeMsg
                , Cmd.batch [ Cmd.map gameMsgWrapper initCmd, loadCss ]
                )
