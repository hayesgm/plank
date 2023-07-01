module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, text)
import Game.TicTacToe.Client as TicTacToe
import Game

import Json.Encode exposing (Value)
import Json.Decode as Decode

type Msg
    = GameMsg Value

type alias GameInst =
    { state : Value
    , update : Value -> Value -> ( Value, Cmd Msg )
    , view : Value -> Html Msg
    , subscriptions : Value -> Sub Msg
    }


initGame : Game.Game state msg -> ( GameInst, Cmd Msg )
initGame game =
    let
        gameMsgWrapper = GameMsg << game.msgEncoder
        ( initState, initCmd ) = game.init
        update_ = \msg statePre ->
            case ( Decode.decodeValue game.msgDecoder msg, Decode.decodeValue game.stateDecoder statePre ) of
                ( Ok msg_, Ok statePre_ ) ->
                    let
                        ( stateNext, cmdNext ) = game.update msg_ statePre_
                    in
                        ( game.stateEncoder stateNext, Cmd.map gameMsgWrapper cmdNext )
                _ ->
                    ( statePre, Cmd.none )

        view_ = Html.map gameMsgWrapper << game.view << Result.withDefault initState << Decode.decodeValue game.stateDecoder
        subscriptions_ = Sub.map gameMsgWrapper << game.subscriptions << Result.withDefault initState << Decode.decodeValue game.stateDecoder
    in
        ( { state = game.stateEncoder initState
          , update = update_
          , view = view_
          , subscriptions = subscriptions_ }, Cmd.map gameMsgWrapper initCmd )



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
    Sub.none


type alias Model =
    { game : Maybe GameInst 
    }


init : ( Model, Cmd Msg )
init =
    let
        ( game, cmd ) = initGame TicTacToe.game
    in
    ( { game = Just game }, cmd )


view : Model -> Html Msg
view model =
    div []
        [ case model.game of
            Just game ->
                game.view game.state

            Nothing ->
                div [] []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameMsg childMsg ->
            case model.game of
                Just game ->
                    let
                        ( stateNext, cmd ) = game.update childMsg game.state
                    in
                        ( { model | game = Just { game | state = stateNext } }, cmd )
                Nothing ->
                    ( model, Cmd.none )
