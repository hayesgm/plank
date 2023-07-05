module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Action
import Browser
import Browser.Navigation as Nav
import Console exposing (log)
import Game
import Game.TicTacToe.Game
import Game.Wordle.Game
import GameInfo
import GameInst exposing (GameInst)
import GameList
import Html exposing (Html, a, button, div, input, text)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Routes exposing (Route)
import Session exposing (Session)
import Url exposing (Url)


type Msg
    = GameState Value
    | InboundMsg Bool Value
    | NewGame GameList.GameName
    | NewGameResp Value
    | GameConnected Value
    | SetInputGameId String
    | JoinGame String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | SetSession (Result Decode.Error Session)
    | ReLogin (Maybe String)


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
    { game : Maybe (GameInst Msg)
    , inputGameId : String
    , assetMapping : String -> Game.AssetMapping
    , session : Maybe Session
    , key : Nav.Key
    , route : Routes.Route
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        assetMapping =
            case Decode.decodeValue (Decode.field "assetMapping" Game.decodeAssetMapping) flags of
                Ok x ->
                    x

                Err err ->
                    \pkg asset -> Nothing

        session =
            case Decode.decodeValue (Decode.field "session" Session.decodeMaybeSession) flags of
                Ok x ->
                    x

                Err err ->
                    Nothing

        route =
            Routes.getRoute url
    in
    ( { game = Nothing
      , inputGameId = ""
      , assetMapping = assetMapping
      , session = session
      , key = key
      , route = route
      }
    , case ( session, route ) of
        ( Just { nonce }, Routes.Game gameId ) ->
            Action.joinGame ( nonce, gameId )

        ( Nothing, _ ) ->
            Session.sessionGuestLogin ()

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
                        div [] [ button [ onClick (NewGame gameName) ] [ text (GameList.gameToString gameName) ] ]
                    )
                    GameList.allGames
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
                        , game.view game.model game.state
                        ]

                    Nothing ->
                        [ div [] [ text "Loading game... " ] ]
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
            case model.session of
                Just { nonce } ->
                    ( model, Action.newGame ( nonce, GameList.gameToString gameName ) )

                _ ->
                    ( model, Cmd.none )

        JoinGame "" ->
            ( model, Cmd.none )

        JoinGame gameId ->
            case model.session of
                Just { nonce } ->
                    ( model, Cmd.batch [ Action.joinGame ( nonce, gameId ), navigateToGame model.key gameId ] )

                _ ->
                    ( model, Cmd.none )

        NewGameResp v ->
            case ( model.session, Decode.decodeValue (Decode.field "gameId" Decode.string) v ) of
                ( Just { nonce }, Ok gameId ) ->
                    ( model, Cmd.batch [ Action.joinGame ( nonce, gameId ), navigateToGame model.key gameId ] )

                ( _, Err err ) ->
                    ( model, log ("Error new game response: " ++ Decode.errorToString err) )

                _ ->
                    ( model, Cmd.none )

        GameConnected gameInfoVal ->
            case Decode.decodeValue GameInfo.gameInfoDecoder gameInfoVal of
                Ok gameInfo ->
                    case GameList.initGame InboundMsg gameInfo.gameName gameInfo.gameId gameInfo.gameState gameInfo.playerId model.assetMapping of
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
                    ( { model | game = Just { game | state = state } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InboundMsg externalMsg inboundMsg ->
            case model.game of
                Just game ->
                    -- This message comes from receiveAction
                    case game.update (externalMsg == False) inboundMsg game.model game.state of
                        Ok ( gameModelNext, gameStateNext, cmd ) ->
                            ( { model
                                | game =
                                    Just
                                        { game
                                            | model = Maybe.withDefault game.model gameModelNext
                                            , state = Maybe.withDefault game.state gameStateNext
                                        }
                              }
                            , cmd
                            )

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
            let
                cmd =
                    case model.game of
                        Just game ->
                            Action.disconnect ()

                        _ ->
                            Cmd.none
            in
            ( { model | route = Routes.getRoute url, game = Nothing }
            , cmd
            )

        SetSession (Err err) ->
            ( model, log (Decode.errorToString err) )

        SetSession (Ok session) ->
            let
                cmd =
                    case model.route of
                        Routes.Game gameId ->
                            Action.joinGame ( session.nonce, gameId )

                        _ ->
                            Cmd.none
            in
            ( { model | session = Just session }, cmd )

        ReLogin maybeGameName ->
            -- TODO: Try to connect game again
            -- TODO: Careful, since this can loop for so many reasons
            ( { model | session = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Action.receiveState GameState
        , Action.receiveAction (encodeInboundMsg >> InboundMsg True)
        , Action.gameConnected GameConnected
        , Action.newGameResp NewGameResp
        , Session.sessionReceive (Decode.decodeValue Session.decodeSession >> SetSession)
        , Session.sessionInvalid ReLogin
        , case model.game of
            Just game ->
                game.subscriptions game.model game.state

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
