module GameInfo exposing (GameInfo, gameInfoDecoder, gameNameDecoder)

import GameList exposing (GameName)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


type alias GameInfo =
    { gameName : GameName
    , gameId : String
    , gameState : Value
    , playerId : String
    }


gameNameDecoder : Decoder GameName
gameNameDecoder =
    Decode.string
        |> Decode.andThen
            (\n ->
                case GameList.gameFromString n of
                    Just g ->
                        Decode.succeed g

                    _ ->
                        Decode.fail ("Unknown game " ++ n)
            )


gameInfoDecoder : Decoder GameInfo
gameInfoDecoder =
    Decode.map4 GameInfo
        (Decode.field "gameName" gameNameDecoder)
        (Decode.field "gameId" Decode.string)
        (Decode.field "gameState" Decode.value)
        (Decode.field "playerId" Decode.string)
