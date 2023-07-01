module Game.TicTacToe.Game exposing (Model, Msg(..), game, init, subscriptions, update, view)

import Browser
import Game exposing (GameMsg(..))
import Game.TicTacToe.Engine as Engine exposing (State, init)
import Html exposing (Html, button, div, input, span, text)
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
            Engine.msgEncoder action


msgDecoder : Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.map GameAction Engine.msgDecoder
        , Decode.map Rename (Decode.field "rename" Decode.string)
        ]


stateEncoder : Model -> Value
stateEncoder model =
    Encode.object
        [ ( "state", Engine.stateEncoder model.state )
        , ( "name", Encode.string model.name )
        ]


stateDecoder : Decoder Model
stateDecoder =
    Decode.map2 Model
        (Decode.field "state" Engine.stateDecoder)
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
            Engine.init
    in
    ( Model state "tic tac toe", Cmd.none )


type Msg
    = Rename String
    | GameAction Engine.InternalMsg


showTile : Int -> Engine.Tile -> Html Msg
showTile pos tile =
    case tile of
        Engine.Open ->
            span [ onClick (GameAction (Engine.Claim pos)) ] [ text " [ ] " ]

        Engine.Taken Engine.X ->
            span [] [ text " [X] " ]

        Engine.Taken Engine.O ->
            span [] [ text " [O] " ]


view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , div [] (List.indexedMap showTile model.state.tiles)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rename str ->
            ( { model | name = str }, Cmd.none )

        GameAction action ->
            let
                ( state_, gameCmd ) =
                    Engine.update (Game.PlayerMsg action) model.state
            in
            ( { model | state = state_ }, Cmd.none )
