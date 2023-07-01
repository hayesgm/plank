module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Action
import Browser
import Console exposing (log)
import Game
import Game.Checkers.Game as Checkers
import Game.TicTacToe.Game as TicTacToe
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode exposing (Value)


type Msg
    = GameState Value
    | GameMsg Value
    | NewGame Game.GameName
    | NewGameResp Value
    | GameConnected Value


type alias GameInst =
    { state : Value
    , update : Value -> Value -> Result String ( Value, Cmd Msg )
    , view : Value -> Html Msg
    , subscriptions : Value -> Sub Msg
    , gameId : String
    , playerId : String
    }



-- TODO: Test early with a second game type


initGame : Game.GameName -> String -> String -> ( GameInst, Cmd Msg )
initGame gameName gameId playerId =
    case gameName of
        Game.TicTacToe ->
            initGameInst TicTacToe.game gameId playerId

        Game.Checkers ->
            initGameInst Checkers.game gameId playerId


initGameInst : Game.Game state msg -> String -> String -> ( GameInst, Cmd Msg )
initGameInst game gameId playerId =
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
                        Ok ( game.stateEncoder stateNext, Cmd.batch [ Cmd.map gameMsgWrapper cmdNext, Action.sendAction msg ] )

                    ( Err msgErr, _ ) ->
                        Err ("Error decoding msg " ++ Decode.errorToString msgErr)

                    ( _, Err stateErr ) ->
                        Err ("Error decoding state " ++ Decode.errorToString stateErr)

        view_ =
            Html.map gameMsgWrapper << game.view << Result.withDefault initState << Decode.decodeValue game.stateDecoder

        subscriptions_ =
            Sub.map gameMsgWrapper << game.subscriptions << Result.withDefault initState << Decode.decodeValue game.stateDecoder
    in
    ( { state = game.stateEncoder initState
      , update = update_
      , view = view_
      , subscriptions = subscriptions_
      , gameId = gameId
      , playerId = playerId
      }
    , Cmd.map gameMsgWrapper initCmd
    )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Action.receiveState GameState
        , Action.receiveAction GameMsg
        , Action.gameConnected GameConnected
        , Action.newGameResp NewGameResp
        ]


type alias Model =
    { game : Maybe GameInst
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.game of
            Just game ->
                game.view game.state

            Nothing ->
                div []
                    (List.map
                        (\gameName ->
                            div [] [ button [ onClick (NewGame gameName) ] [ text (Game.gameToString gameName) ] ]
                        )
                        Game.allGames
                    )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame gameName ->
            ( model, Action.newGame (Game.gameToString gameName) )

        NewGameResp v ->
            case Decode.decodeValue (Decode.field "gameId" Decode.string) v of
                Ok gameId ->
                    ( model, Action.joinGame gameId )

                Err err ->
                    ( model, log ("Error new game response: " ++ Decode.errorToString err) )

        GameConnected gameInfoVal ->
            case Decode.decodeValue Game.gameInfoDecoder gameInfoVal of
                Ok gameInfo ->
                    let
                        ( gameInst, cmd ) =
                            initGame gameInfo.gameName gameInfo.gameId gameInfo.playerId
                    in
                    ( { model | game = Just gameInst }, cmd )

                Err err ->
                    ( model, log ("Error decoding game info " ++ Decode.errorToString err) )

        GameState state ->
            case model.game of
                Just game ->
                    ( { model | game = Just { game | state = state } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GameMsg childMsg ->
            case model.game of
                Just game ->
                    case game.update childMsg game.state of
                        Ok ( stateNext, cmd ) ->
                            ( { model | game = Just { game | state = stateNext } }, cmd )

                        Err err ->
                            ( model, log err )

                Nothing ->
                    ( model, Cmd.none )
