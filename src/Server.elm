module Server exposing (Model, Msg(..), init, main, subscriptions, update)

import Action exposing (giveState, receiveAction)
import Console exposing (log)
import Game exposing (GameMsg(..))
import Game.TicTacToe.Engine as TicTacToe
import Json.Decode as Decode
import Json.Encode exposing (Value)
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


initGameInst : Game.Engine state msg -> ( GameInst, Value, Cmd Msg )
initGameInst game =
    let
        gameMsgWrapper =
            GameMsg Nothing << game.msgEncoder

        ( initState, initCmd ) =
            game.init

        update_ =
            \msg maybePlayerId statePre ->
                case ( Decode.decodeValue game.msgDecoder msg, Decode.decodeValue game.stateDecoder statePre ) of
                    ( Ok msg_, Ok statePre_ ) ->
                        let
                            gameMsg =
                                case maybePlayerId of
                                    Just playerId ->
                                        PlayerMsg playerId msg_

                                    Nothing ->
                                        SystemMsg msg_

                            ( stateNext, cmdNext ) =
                                game.update gameMsg statePre_
                        in
                        Ok ( game.stateEncoder stateNext, game.publicStateEncoder stateNext, Cmd.map gameMsgWrapper cmdNext )

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
                            if game.state /= stateNext then
                                ( { model | game = Just { game | state = stateNext } }, Cmd.batch [ giveState publicStateNext, cmd ] )

                            else
                                ( model, cmd )

                        Err err ->
                            ( model, log err )

        Nothing ->
            ( model, Cmd.none )
