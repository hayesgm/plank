module GameList exposing (GameName(..), allGames, gameFromString, gameToString)

import Game.TicTacToe.Engine
import Game.Wordle.Engine


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
