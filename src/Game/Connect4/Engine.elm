module Game.Connect4.Engine exposing (engine, update)

import Dict
import Game exposing (GameMsg(..), PlayerId)
import Game.Connect4.Helpers exposing (checkWinner, otherPlayer)
import Game.Connect4.Types as Types exposing (EngineMsg(..), Player(..), State, Tile(..))
import List.Extra as ListEx
import Time


engine : Game.Engine State EngineMsg
engine =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = Types.encodeEngineMsg
    , msgDecoder = Types.decodeEngineMsg
    , stateEncoder = Types.encodeState
    , stateDecoder = Types.decodeState
    , getPublicState = identity
    }


init : ( State, Cmd EngineMsg )
init =
    ( { turn = X
      , tiles = List.repeat 42 Open
      , winner = Nothing
      , players = Dict.empty
      }
    , Cmd.none
    )


subscriptions : State -> Sub EngineMsg
subscriptions _ =
    Time.every 10000 Types.Tick


updateInternal : EngineMsg -> Maybe PlayerId -> State -> ( State, Cmd EngineMsg )
updateInternal msg maybePlayerId state =
    case msg of
        Types.Tick _ ->
            ( state, Cmd.none )

        JoinGame ->
            let
                newPlayers =
                    case maybePlayerId of
                        Just playerId ->
                            case Dict.get playerId state.players of
                                Nothing ->
                                    case Dict.size state.players of
                                        0 ->
                                            Dict.insert playerId X state.players

                                        1 ->
                                            Dict.insert playerId O state.players

                                        _ ->
                                            state.players

                                _ ->
                                    state.players

                        _ ->
                            state.players
            in
            ( { state | players = newPlayers }, Cmd.none )

        Claim x ->
            case ( maybePlayerId, state.winner ) of
                ( Just playerId, Nothing ) ->
                    case Dict.get playerId state.players of
                        Just player ->
                            if state.turn == player then
                                case ListEx.getAt x state.tiles of
                                    Just Open ->
                                        let
                                            tiles =
                                                ListEx.setAt x (Taken player) state.tiles

                                            maybeWinner =
                                                checkWinner tiles
                                        in
                                        ( { state | tiles = tiles, turn = otherPlayer state.turn, winner = maybeWinner }, Cmd.none )

                                    _ ->
                                        ( state, Cmd.none )

                            else
                                ( state, Cmd.none )

                        _ ->
                            ( state, Cmd.none )

                _ ->
                    ( state, Cmd.none )


update : Game.EngineMsg EngineMsg -> State -> ( State, Cmd EngineMsg )
update msg model =
    case msg of
        Game.PlayerMsg playerId internalMsg ->
            updateInternal internalMsg (Just playerId) model

        Game.SystemMsg internalMsg ->
            updateInternal internalMsg Nothing model
