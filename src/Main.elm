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
    | GameMsg (Maybe String) Value
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


initGame : Game.GameName -> String -> Value -> String -> (String -> Game.AssetMapping) -> Result String ( GameInst, Maybe Msg, Cmd Msg )
initGame gameName gameId gameState playerId assetMapping =
    case gameName of
        Game.TicTacToe ->
            initGameInst TicTacToe.game gameName gameId gameState playerId assetMapping


initGameInst : Game.Game model state msg -> Game.GameName -> String -> Value -> String -> (String -> Game.AssetMapping) -> Result String ( GameInst, Maybe Msg, Cmd Msg )
initGameInst game gameName gameId gameState playerId assetMapping =
    case Decode.decodeValue game.stateDecoder gameState of
        Err err ->
            Err (Decode.errorToString err)

        Ok initState ->
            let
                gameMsgWrapper =
                    GameMsg (Just playerId) << game.msgEncoder

                ( initModel, maybeMsg, initCmd ) =
                    game.init playerId initState

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

                gameAssetMapping =
                    assetMapping (Game.gameReflect gameName)

                view_ =
                    Html.map gameMsgWrapper << game.view gameAssetMapping << Result.withDefault initModel << Decode.decodeValue game.modelDecoder

                subscriptions_ =
                    Sub.map gameMsgWrapper << game.subscriptions << Result.withDefault initModel << Decode.decodeValue game.modelDecoder

                loadCss =
                    case game.css gameAssetMapping of
                        Just asset ->
                            Action.loadCss asset

                        _ ->
                            Cmd.none
            in
            Ok
                ( { model = game.modelEncoder initModel
                  , update = update_
                  , view = view_
                  , subscriptions = subscriptions_
                  , setGameState = setGameState_
                  , gameId = gameId
                  , playerId = playerId
                  }
                , Maybe.map gameMsgWrapper maybeMsg
                , Cmd.batch [ Cmd.map gameMsgWrapper initCmd, loadCss ]
                )


main : Program Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Action.receiveState GameState
        , Action.receiveAction (\( playerId, action ) -> GameMsg playerId action)
        , Action.gameConnected GameConnected
        , Action.newGameResp NewGameResp
        , case model.game of
            Just game ->
                game.subscriptions game.model

            _ ->
                Sub.none
        ]


type alias Model =
    { game : Maybe GameInst
    , joinGameId : String
    , assetMapping : String -> Game.AssetMapping
    }


init : Value -> ( Model, Cmd Msg )
init assets =
    let
        assetMapping =
            case Decode.decodeValue Game.decodeAssetMapping assets of
                Ok x ->
                    x

                Err err ->
                    \pkg asset -> Nothing
    in
    ( { game = Nothing, joinGameId = "", assetMapping = assetMapping }, Cmd.none )


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
                    case initGame gameInfo.gameName gameInfo.gameId gameInfo.gameState gameInfo.playerId model.assetMapping of
                        Ok ( gameInst, maybeMsg, cmd ) ->
                            case maybeMsg of
                                Just msg_ ->
                                    let
                                        ( model_, cmd_ ) =
                                            update msg_ { model | game = Just gameInst }
                                    in
                                    ( model_, Cmd.batch [ cmd, cmd_ ] )

                                Nothing ->
                                    ( { model | game = Just gameInst }, cmd )

                        Err err ->
                            ( model, log ("Error initializing game " ++ err) )

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

        GameMsg maybePlayerId childMsg ->
            -- TODO: Don't ignore "maybePlayerId" here
            case model.game of
                Just game ->
                    case game.update childMsg game.model of
                        Ok ( gameModelNext, cmd ) ->
                            ( { model | game = Just { game | model = gameModelNext } }, cmd )

                        Err err ->
                            ( model, log err )

                Nothing ->
                    ( model, Cmd.none )
