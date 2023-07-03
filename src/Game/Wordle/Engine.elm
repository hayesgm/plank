module Game.Wordle.Engine exposing (engine, update)

import Dict exposing (Dict)
import Game exposing (GameMsg(..), PlayerId)
import Game.Wordle.Helpers as Helpers exposing (Status(..), annotateGuess, append, getStatus)
import Game.Wordle.Ports as Ports
import Game.Wordle.Types as Types exposing (EngineMsg(..), State)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as ListEx



-- TODO: Check words in dictionary (seen here: https://github.com/tabatkins/wordle-list/blob/main/words)
-- TODO: Figure out yellow squares


engine : Game.Engine State EngineMsg
engine =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = Types.encodeEngineMsg
    , msgDecoder = Types.decodeEngineMsg
    , stateEncoder = Types.encodeState
    , stateDecoder = Types.decodeState
    , getPublicState = getPublicState
    }


init : ( State, Cmd EngineMsg )
init =
    ( { player = Nothing
      , guesses = []
      , target = Nothing
      }
    , Ports.wordleFetch ()
    )


subscriptions : State -> Sub EngineMsg
subscriptions model =
    Ports.wordleGot SetTarget


updateInternal : EngineMsg -> Maybe PlayerId -> State -> ( State, Cmd EngineMsg )
updateInternal msg maybePlayerId state =
    case ( state.target, state.player, msg ) of
        ( Just target, Just _, Guess guess ) ->
            if state.player == maybePlayerId then
                case ( getStatus state.guesses, annotateGuess target guess ) of
                    ( Playing, Ok annotated ) ->
                        ( { state | guesses = append annotated state.guesses }, Cmd.none )

                    _ ->
                        ( state, Cmd.none )

            else
                ( state, Cmd.none )

        ( _, _, SetTarget t ) ->
            ( { state | target = Just t }, Cmd.none )

        ( _, Nothing, JoinGame ) ->
            ( { state | player = maybePlayerId }, Cmd.none )

        _ ->
            ( state, Cmd.none )



-- TODO: Should this just be moved to the boilerplate?


update : Game.EngineMsg EngineMsg -> State -> ( State, Cmd EngineMsg )
update msg model =
    case msg of
        Game.PlayerMsg playerId internalMsg ->
            updateInternal internalMsg (Just playerId) model

        Game.SystemMsg internalMsg ->
            updateInternal internalMsg Nothing model


getPublicState : State -> State
getPublicState state =
    { state | target = Nothing }
