module Server exposing (Model, Msg(..), init, main, subscriptions, update)

import Action exposing (giveState, receiveAction)
import Console exposing (log)
import Game exposing (EngineMsg(..))
import Game.TicTacToe.Engine
import Game.Wordle.Engine
import GameList
import GameServer exposing (GameServer)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Platform exposing (worker)


type Msg
    = GameMsg (Maybe String) Value


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
    { game : Maybe (GameServer Msg)
    }


init : String -> ( Model, Cmd Msg )
init gameNameStr =
    case GameList.gameFromString gameNameStr of
        Just gameName ->
            let
                ( game, publicState, cmd ) =
                    GameList.initEngine GameMsg gameName
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
