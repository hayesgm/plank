module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Action
import Browser
import Console exposing (log)
import Game
import Game.TicTacToe.Game as TicTacToe
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode exposing (Value)


type Msg
    = GameState Value
    | GameMsg Value
    | NewGame Game.GameName
    | NewGameResp Value
    | GameConnected Value
    | SetJoinGameId String
    | JoinGame String


type alias GameInst =
    { model : Value
    , update : Value -> Value -> Result String ( Value, Cmd Msg )
    , view : Value -> Html Msg
    , subscriptions : Value -> Sub Msg
    , setGameState : Value -> Value -> Result String Value
    , gameId : String
    , playerId : String
    }



-- TODO: Test early with a second game type


initGame : Game.GameName -> String -> String -> ( GameInst, Cmd Msg )
initGame gameName gameId playerId =
    case gameName of
        Game.TicTacToe ->
            initGameInst TicTacToe.game gameId playerId


initGameInst : Game.Game model state msg -> String -> String -> ( GameInst, Cmd Msg )
initGameInst game gameId playerId =
    let
        gameMsgWrapper =
            GameMsg << game.msgEncoder

        ( initModel, initCmd ) =
            game.init

        update_ =
            \msg modelPre ->
                case ( Decode.decodeValue game.msgDecoder msg, Decode.decodeValue game.modelDecoder modelPre ) of
                    ( Ok msg_, Ok modelPre_ ) ->
                        let
                            ( modelNext, cmdNext ) =
                                game.update msg_ modelPre_
                        in
                        Ok ( game.modelEncoder modelNext, Cmd.batch [ Cmd.map gameMsgWrapper cmdNext, Action.sendAction msg ] )

                    ( Err msgErr, _ ) ->
                        Err ("Error decoding msg " ++ Decode.errorToString msgErr)

                    ( _, Err modelErr ) ->
                        Err ("Error decoding model " ++ Decode.errorToString modelErr)

        setGameState_ =
            \state modelPre ->
                case ( Decode.decodeValue game.stateDecoder state, Decode.decodeValue game.modelDecoder modelPre ) of
                    ( Ok state_, Ok modelPre_ ) ->
                        Ok (game.modelEncoder (game.setGameState state_ modelPre_))

                    ( Err stateErr, _ ) ->
                        Err ("Error decoding state " ++ Decode.errorToString stateErr)

                    ( _, Err modelErr ) ->
                        Err ("Error decoding model " ++ Decode.errorToString modelErr)

        view_ =
            Html.map gameMsgWrapper << game.view << Result.withDefault initModel << Decode.decodeValue game.modelDecoder

        subscriptions_ =
            Sub.map gameMsgWrapper << game.subscriptions << Result.withDefault initModel << Decode.decodeValue game.modelDecoder
    in
    ( { model = game.modelEncoder initModel
      , update = update_
      , view = view_
      , subscriptions = subscriptions_
      , setGameState = setGameState_
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
    , joinGameId : String
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Nothing, joinGameId = "" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.game of
            Just game ->
                div []
                    [ text game.gameId
                    , game.view game.model
                    ]

            Nothing ->
                div []
                    [ div []
                        (List.map
                            (\gameName ->
                                div [] [ button [ onClick (NewGame gameName) ] [ text (Game.gameToString gameName) ] ]
                            )
                            Game.allGames
                        )
                    , div []
                        [ input [ placeholder "game_...", value model.joinGameId, onInput SetJoinGameId ] []
                        , button [ onClick (JoinGame model.joinGameId) ] [ text "Join Game" ]
                        ]
                    ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetJoinGameId joinGameId ->
            ( { model | joinGameId = joinGameId }, Cmd.none )

        NewGame gameName ->
            ( model, Action.newGame (Game.gameToString gameName) )

        JoinGame "" ->
            ( model, Cmd.none )

        JoinGame gameId ->
            ( model, Action.joinGame gameId )

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
                    case game.setGameState state game.model of
                        Ok gameModelNext ->
                            ( { model | game = Just { game | model = gameModelNext } }, Cmd.none )

                        Err err ->
                            ( model, log err )

                Nothing ->
                    ( model, Cmd.none )

        GameMsg childMsg ->
            case model.game of
                Just game ->
                    case game.update childMsg game.model of
                        Ok ( gameModelNext, cmd ) ->
                            ( { model | game = Just { game | model = gameModelNext } }, cmd )

                        Err err ->
                            ( model, log err )

                Nothing ->
                    ( model, Cmd.none )
