module Game.TicTacToe.Game exposing (Model, Msg(..), game, init, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), PlayerId)
import Game.TicTacToe.Engine as Engine exposing (Player(..), State, init)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


game : Game.Game Model State Msg
game =
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = msgEncoder
    , msgDecoder = msgDecoder
    , modelEncoder = modelEncoder
    , modelDecoder = modelDecoder
    , stateEncoder = Engine.stateEncoder
    , stateDecoder = Engine.stateDecoder
    , setGameState = setGameState
    }



-- TODO: Handle differentiating action types better!


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


modelEncoder : Model -> Value
modelEncoder model =
    Encode.object
        [ ( "state", Engine.stateEncoder model.state )
        , ( "playerId", Encode.string model.playerId )
        , ( "name", Encode.string model.name )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.map3 Model
        (Decode.field "state" Engine.stateDecoder)
        (Decode.field "playerId" Decode.string)
        (Decode.field "name" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { state : State
    , playerId : PlayerId
    , name : String
    }


init : PlayerId -> State -> ( Model, Maybe Msg, Cmd Msg )
init playerId state =
    let
        msg =
            if Dict.size state.players < 2 then
                Just (GameAction Engine.JoinGame)

            else
                Nothing
    in
    ( Model state playerId "tic tac toe", msg, Cmd.none )


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


showPlayer : Player -> String
showPlayer p =
    case p of
        X ->
            "X"

        O ->
            "O"


descPlayer : PlayerId -> Dict PlayerId Player -> String
descPlayer p players =
    case Dict.get p players of
        Just player ->
            "Playing as " ++ showPlayer player

        _ ->
            "Spectating"


view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , text (descPlayer model.playerId model.state.players)
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
                    Engine.update (Game.PlayerMsg model.playerId action) model.state
            in
            ( { model | state = state_ }, Cmd.none )


setGameState : State -> Model -> Model
setGameState state model =
    { model | state = state }
