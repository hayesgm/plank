module Game.TicTacToe.Client exposing (Model, Msg(..), game, init, subscriptions, update, view)

import Browser
import Game exposing (GameMsg(..))
import Game.TicTacToe.Main exposing (State, init)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


game : Game.Game Model Msg
game =
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = msgEncoder
    , msgDecoder = msgDecoder
    , stateEncoder = stateEncoder
    , stateDecoder = stateDecoder
    }


msgEncoder : Msg -> Value
msgEncoder msg =
    case msg of
        Rename s ->
            Encode.object [ ( "rename", Encode.string s ) ]

        GameAction action ->
            Game.TicTacToe.Main.msgEncoder action


msgDecoder : Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.map GameAction Game.TicTacToe.Main.msgDecoder
        , Decode.map Rename (Decode.field "rename" Decode.string)
        ]


stateEncoder : Model -> Value
stateEncoder model =
    Encode.object
        [ ( "state", Game.TicTacToe.Main.stateEncoder model.state )
        , ( "name", Encode.string model.name )
        ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.map2 Model
        (Decode.field "state" Game.TicTacToe.Main.stateDecoder)
        (Decode.field "name" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { state : State
    , name : String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( state, _ ) =
            Game.TicTacToe.Main.init
    in
    ( Model state "tic tac toe", Cmd.none )


type Msg
    = Rename String
    | GameAction Game.TicTacToe.Main.InternalMsg


view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , text (String.fromInt model.state.ticks)
        , text (String.join "," (List.map String.fromInt model.state.pings))
        , button [ onClick (GameAction (Game.TicTacToe.Main.Ping 5)) ] [ text "Ping" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rename str ->
            ( { model | name = str }, Cmd.none )

        GameAction action ->
            let
                ( state_, gameCmd ) =
                    Game.TicTacToe.Main.update (Game.PlayerMsg action) model.state
            in
            ( { model | state = state_ }, Cmd.none )



-- TODO: Handle game commands?
