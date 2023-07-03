module GameList exposing (GameName(..), allGames, gameFromString, gameToString, initEngine, initGame)

import Game
import Game.TicTacToe.Engine
import Game.TicTacToe.Game
import Game.Wordle.Engine
import Game.Wordle.Game
import GameInst exposing (GameInst, MsgWrapper, initGameInst)
import GameServer exposing (GameMsgWrapper, GameServer, initGameServer)
import Json.Encode exposing (Value)


type GameName
    = TicTacToe
    | Wordle


allGames : List GameName
allGames =
    [ TicTacToe, Wordle ]


gameToString : GameName -> String
gameToString gameName =
    case gameName of
        TicTacToe ->
            "TicTacToe"

        Wordle ->
            "Wordle"


gameFromString : String -> Maybe GameName
gameFromString gameName =
    case gameName of
        "TicTacToe" ->
            Just TicTacToe

        "Wordle" ->
            Just Wordle

        _ ->
            Nothing


initEngine : GameMsgWrapper serverMsg -> GameName -> ( GameServer serverMsg, Value, Cmd serverMsg )
initEngine msgWrapper gameName =
    case gameName of
        TicTacToe ->
            initGameServer Game.TicTacToe.Engine.engine msgWrapper

        Wordle ->
            initGameServer Game.Wordle.Engine.engine msgWrapper


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
        TicTacToe ->
            initGameInst Game.TicTacToe.Game.game gameInstData

        Wordle ->
            initGameInst Game.Wordle.Game.game gameInstData
