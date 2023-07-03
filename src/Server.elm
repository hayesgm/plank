module Server exposing (Model, Msg(..), init, main, subscriptions, update)

import Action exposing (giveState, receiveAction)
import Console exposing (log)
import Game exposing (EngineMsg(..))
import Game.TicTacToe.Engine as TicTacToe
import Game.Wordle.Engine as Wordle
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Platform exposing (worker)


type Msg
    = GameMsg (Maybe String) Value


type alias GameInst =
    { state : Value
    , update : Value -> Maybe String -> Value -> Result String ( Value, Value, Cmd Msg )
    , subscriptions : Value -> Sub Msg
    }


initGame : Game.GameName -> ( GameInst, Value, Cmd Msg )
initGame gameName =
    case gameName of
        Game.TicTacToe ->
            initGameInst TicTacToe.engine

        Game.Wordle ->
            initGameInst Wordle.engine


initGameInst : Game.Engine state msg -> ( GameInst, Value, Cmd Msg )
initGameInst game =
    let
        gameMsgWrapper =
            GameMsg Nothing << game.msgEncoder

        ( initState, initCmd ) =
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
                        Ok ( game.stateEncoder stateNext, game.publicStateEncoder (game.getPublicState stateNext), Cmd.map gameMsgWrapper cmdNext )

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
    , game.publicStateEncoder initState
    , Cmd.map gameMsgWrapper initCmd
    )


main : Program String Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveAction (\( playerId, action ) -> GameMsg playerId action)
        , case model.game of
            Just game ->
                game.subscriptions game.state

            _ ->
                Sub.none
        ]


type alias Model =
    { game : Maybe GameInst
    }


init : String -> ( Model, Cmd Msg )
init gameNameStr =
    case Game.gameFromString gameNameStr of
        Just gameName ->
            let
                ( game, publicState, cmd ) =
                    initGame gameName
            in
            ( { game = Just game }, Cmd.batch [ giveState publicState, cmd ] )

        Nothing ->
            ( { game = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.game of
        Just game ->
            case msg of
                GameMsg playerId childMsg ->
                    case game.update childMsg playerId game.state of
                        Ok ( stateNext, publicStateNext, cmd ) ->
                            -- TODO: Better way to compare versus encoding
                            if Encode.encode 0 stateNext /= Encode.encode 0 game.state then
                                ( { model | game = Just { game | state = stateNext } }, Cmd.batch [ giveState publicStateNext, cmd ] )

                            else
                                ( { model | game = Just { game | state = stateNext } }, cmd )

                        Err err ->
                            ( model, log err )

        Nothing ->
            ( model, Cmd.none )
