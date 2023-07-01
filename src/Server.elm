module Server exposing (Model, Msg(..), init, main, subscriptions, update)

import Action exposing (giveState, receiveAction)
import Browser
import Console exposing (log)
import Game
import Game.TicTacToe.Main as TicTacToe
import Html exposing (Html, div, text)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import Platform exposing (worker)


type Msg
    = GameMsg Value


type alias GameInst =
    { state : Value
    , update : Value -> Value -> Result String ( Value, Cmd Msg )
    , subscriptions : Value -> Sub Msg
    }


initGame : Game.GameServer state msg -> ( GameInst, Cmd Msg )
initGame game =
    let
        gameMsgWrapper =
            GameMsg << game.msgEncoder

        ( initState, initCmd ) =
            game.init

        update_ =
            \msg statePre ->
                case ( Decode.decodeValue game.msgDecoder msg, Decode.decodeValue game.stateDecoder statePre ) of
                    ( Ok msg_, Ok statePre_ ) ->
                        let
                            ( stateNext, cmdNext ) =
                                game.update msg_ statePre_
                        in
                        Ok ( game.stateEncoder stateNext, Cmd.map gameMsgWrapper cmdNext )

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
    , Cmd.map gameMsgWrapper initCmd
    )


main : Program () Model Msg
main =
    worker
        { init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveAction GameMsg
        ]


type alias Model =
    { game : GameInst
    }


init : ( Model, Cmd Msg )
init =
    let
        ( game, cmd ) =
            initGame TicTacToe.game
    in
    ( { game = game }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        GameMsg childMsg ->
            case game.update childMsg game.state of
                Ok ( stateNext, cmd ) ->
                    ( { model | game = { game | state = stateNext } }, Cmd.batch [ giveState stateNext, cmd ] )

                Err err ->
                    ( model, log err )
