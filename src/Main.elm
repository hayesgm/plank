module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Action
import Browser
import Browser.Navigation as Nav
import Console exposing (log)
import Game
import Game.TicTacToe.Game as TicTacToe
import Game.Wordle.Game as Wordle
import Html exposing (Html, a, button, div, input, text)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Routes exposing (Route)
import Url exposing (Url)


type Msg
    = GameState Value
    | InboundMsg Bool Value
    | NewGame Game.GameName
    | NewGameResp Value
    | GameConnected Value
    | SetInputGameId String
    | JoinGame String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


type alias GameInst =
    { model : Value
    , update : Bool -> Value -> Value -> Result String ( Value, Cmd Msg )
    , view : Value -> Html Msg
    , subscriptions : Value -> Sub Msg
    , setGameState : Value -> Value -> Result String Value
    , gameId : String
    , playerId : String
    }


main : Program Value Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { game : Maybe GameInst
    , inputGameId : String
    , assetMapping : String -> Game.AssetMapping
    , key : Nav.Key
    , route : Routes.Route
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        assetMapping =
            case Decode.decodeValue Game.decodeAssetMapping flags of
                Ok x ->
                    x

                Err err ->
                    \pkg asset -> Nothing

        route =
            Routes.getRoute url
    in
    ( { game = Nothing
      , inputGameId = ""
      , assetMapping = assetMapping
      , key = key
      , route = route
      }
    , case route of
        Routes.Game gameId ->
            Action.joinGame gameId

        _ ->
            Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Routes.Home ->
            viewHome model

        Routes.Game gameId ->
            viewGame gameId model

        Routes.NotFound ->
            viewNotFound model


viewHome : Model -> Browser.Document Msg
viewHome model =
    { title = "Plank"
    , body =
        [ div []
            [ div []
                (List.map
                    (\gameName ->
                        div [] [ button [ onClick (NewGame gameName) ] [ text (Game.gameToString gameName) ] ]
                    )
                    Game.allGames
                )
            , div []
                [ input [ placeholder "game_...", value model.inputGameId, onInput SetInputGameId ] []
                , button [ onClick (JoinGame model.inputGameId) ] [ text "Join Game" ]
                ]
            ]
        ]
    }


viewGame : String -> Model -> Browser.Document Msg
viewGame gameId model =
    { title = "Plank " ++ gameId
    , body =
        [ a [ href "/" ] [ text "Home 🪵" ] ]
            ++ (case model.game of
                    Just game ->
                        [ text game.gameId
                        , game.view game.model
                        ]

                    Nothing ->
                        [ viewNotFoundHtml model ]
               )
    }


viewNotFound : Model -> Browser.Document Msg
viewNotFound model =
    { title = "Plank - Not Found"
    , body = [ a [ href "/" ] [ text "Home 🪵" ] ] ++ [ viewNotFoundHtml model ]
    }


viewNotFoundHtml : Model -> Html Msg
viewNotFoundHtml model =
    div [] [ text "Page not found" ]


navigateToGame : Nav.Key -> String -> Cmd Msg
navigateToGame key gameId =
    Nav.pushUrl key (Routes.getUrl (Routes.Game gameId))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetInputGameId inputGameId ->
            ( { model | inputGameId = inputGameId }, Cmd.none )

        NewGame gameName ->
            ( model, Action.newGame (Game.gameToString gameName) )

        JoinGame "" ->
            ( model, Cmd.none )

        JoinGame gameId ->
            ( model, Cmd.batch [ Action.joinGame gameId, navigateToGame model.key gameId ] )

        NewGameResp v ->
            case Decode.decodeValue (Decode.field "gameId" Decode.string) v of
                Ok gameId ->
                    ( model, Cmd.batch [ Action.joinGame gameId, navigateToGame model.key gameId ] )

                Err err ->
                    ( model, log ("Error new game response: " ++ Decode.errorToString err) )

        GameConnected gameInfoVal ->
            case Decode.decodeValue Game.gameInfoDecoder gameInfoVal of
                Ok gameInfo ->
                    case initGame gameInfo.gameName gameInfo.gameId gameInfo.gameState gameInfo.playerId model.assetMapping of
                        Ok ( gameInst, maybeInitMsg, initGameCmd ) ->
                            case maybeInitMsg of
                                Just initMsg ->
                                    let
                                        ( modelPostUpdate, updateCmd ) =
                                            update initMsg { model | game = Just gameInst }
                                    in
                                    ( modelPostUpdate, Cmd.batch [ initGameCmd, updateCmd ] )

                                Nothing ->
                                    ( { model | game = Just gameInst }, initGameCmd )

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

        InboundMsg externalMsg inboundMsg ->
            case model.game of
                Just game ->
                    -- This message comes from receiveAction
                    case game.update (externalMsg == False) inboundMsg game.model of
                        Ok ( gameModelNext, cmd ) ->
                            ( { model | game = Just { game | model = gameModelNext } }, cmd )

                        Err err ->
                            ( model, log err )

                Nothing ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | route = Routes.getRoute url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Action.receiveState GameState
        , Action.receiveAction (encodeInboundMsg >> InboundMsg True)
        , Action.gameConnected GameConnected
        , Action.newGameResp NewGameResp
        , case model.game of
            Just game ->
                game.subscriptions game.model

            _ ->
                Sub.none
        ]


wrapEngineMsg : Maybe String -> msg -> Game.EngineMsg msg
wrapEngineMsg maybePlayerId msg =
    case maybePlayerId of
        Just playerId ->
            Game.PlayerMsg playerId msg

        Nothing ->
            Game.SystemMsg msg


encodeInboundMsg : ( Maybe Game.PlayerId, Value ) -> Value
encodeInboundMsg ( maybePlayerId, engineMsg ) =
    case maybePlayerId of
        Just playerId ->
            Encode.object
                [ ( "PID", Encode.string playerId )
                , ( "PM", engineMsg )
                ]

        Nothing ->
            Encode.object
                [ ( "SM", engineMsg ) ]


initGame : Game.GameName -> String -> Value -> String -> (String -> Game.AssetMapping) -> Result String ( GameInst, Maybe Msg, Cmd Msg )
initGame gameName gameId gameState playerId assetMapping =
    case gameName of
        Game.TicTacToe ->
            initGameInst TicTacToe.game gameName gameId gameState playerId assetMapping

        Game.Wordle ->
            initGameInst Wordle.game gameName gameId gameState playerId assetMapping


initGameInst : Game.Game model state engineMsg gameMsg -> Game.GameName -> String -> Value -> String -> (String -> Game.AssetMapping) -> Result String ( GameInst, Maybe Msg, Cmd Msg )
initGameInst game gameName gameId gameState playerId assetMapping =
    case Decode.decodeValue game.stateDecoder gameState of
        Err err ->
            Err (Decode.errorToString err)

        Ok initState ->
            let
                ( initModel, maybeMsg, initCmd ) =
                    game.init playerId initState

                inboundMsgDecoder : Decoder (Game.InboundMsg engineMsg gameMsg)
                inboundMsgDecoder =
                    Decode.oneOf
                        [ Decode.map Game.EngineMsg
                            (Decode.map2
                                Game.PlayerMsg
                                (Decode.field "PID" Decode.string)
                                (Decode.field "PM" game.engineMsgDecoder)
                            )
                        , Decode.map Game.EngineMsg (Decode.map Game.SystemMsg (Decode.field "SM" game.engineMsgDecoder))
                        , Decode.map Game.GameMsg (Decode.field "GM" game.gameMsgDecoder)
                        ]

                gameMsgEncoder : Game.GameMsg engineMsg gameMsg -> Value
                gameMsgEncoder gameMsg =
                    case gameMsg of
                        Game.ForEngine engineMsg ->
                            Encode.object
                                [ ( "PID", Encode.string playerId )
                                , ( "PM", game.engineMsgEncoder engineMsg )
                                ]

                        Game.ForSelf gameMsg_ ->
                            Encode.object
                                [ ( "GM", game.gameMsgEncoder gameMsg_ ) ]

                gameMsgWrapper =
                    InboundMsg False << gameMsgEncoder

                update_ =
                    \sendMsg msgEnc modelPreEnc ->
                        case ( Decode.decodeValue inboundMsgDecoder msgEnc, Decode.decodeValue game.modelDecoder modelPreEnc ) of
                            ( Ok inboundMsg, Ok modelPre ) ->
                                let
                                    ( modelNext, cmdNext ) =
                                        game.update inboundMsg modelPre

                                    sendActionCmd =
                                        case ( sendMsg, inboundMsg ) of
                                            ( True, Game.EngineMsg (Game.PlayerMsg playerId_ engineMsg) ) ->
                                                if playerId == playerId_ then
                                                    Action.sendAction (game.engineMsgEncoder engineMsg)

                                                else
                                                    Cmd.none

                                            _ ->
                                                Cmd.none
                                in
                                Ok ( game.modelEncoder modelNext, Cmd.batch [ Cmd.map gameMsgWrapper cmdNext, sendActionCmd ] )

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
