module GameList exposing (GameName(..), allGames, gameFromString, gameToString, initEngine, initGame)

import Game
import Game.Connect4.Engine
import Game.Connect4.Game
import Game.TicTacToe.Engine
import Game.TicTacToe.Game
import Game.Wordle.Engine
import Game.Wordle.Game
import GameInst exposing (GameInst, MsgWrapper, initGameInst)
import GameServer exposing (GameMsgWrapper, GameServer, initGameServer)
import Json.Encode exposing (Value)


type GameName
    = Connect4
    | TicTacToe
    | Wordle


allGames : List GameName
allGames =
    [ Connect4, TicTacToe, Wordle ]


gameToString : GameName -> String
gameToString gameName =
    case gameName of
        Connect4 ->
            "Connect4"

        TicTacToe ->
            "TicTacToe"

        Wordle ->
            "Wordle"


gameFromString : String -> Maybe GameName
gameFromString gameName =
    case gameName of
        "Connect4" ->
            Just Connect4

        "TicTacToe" ->
            Just TicTacToe

        "Wordle" ->
            Just Wordle

        _ ->
            Nothing


initEngine : GameMsgWrapper serverMsg -> GameName -> Maybe Value -> ( GameServer serverMsg, Value, Cmd serverMsg )
initEngine msgWrapper gameName maybeInitState =
    let
        gameServerData =
            { msgWrapper = msgWrapper
            , maybeInitState = maybeInitState
            }
    in
    case gameName of
        Connect4 ->
            initGameServer Game.Connect4.Engine.engine gameServerData

        TicTacToe ->
            initGameServer Game.TicTacToe.Engine.engine gameServerData

        Wordle ->
            initGameServer Game.Wordle.Engine.engine gameServerData


initGame : MsgWrapper msg -> GameName -> String -> Value -> String -> (String -> Game.AssetMapping) -> Result String ( GameInst msg, Maybe msg, Cmd msg )
initGame msgWrapper gameName gameId gameState playerId assetMapping =
    let
        gameInstData =
            { msgWrapper = msgWrapper
            , gameName = gameToString gameName
            , gameId = gameId
            , gameState = gameState
            , playerId = playerId
            , assetMapping = assetMapping
            }
    in
    case gameName of
        Connect4 ->
            initGameInst Game.Connect4.Game.game gameInstData

        TicTacToe ->
            initGameInst Game.TicTacToe.Game.game gameInstData

        Wordle ->
            initGameInst Game.Wordle.Game.game gameInstData
