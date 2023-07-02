module Game.TicTacToe.Game exposing (Model, Msg(..), game, init, subscriptions, update, view)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), PlayerId)
import Game.TicTacToe.Engine as Engine exposing (Player(..), State, init)
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time


game : Game.Game Model State Msg
game =
    { view = view
    , css = css
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
        Tock t ->
            Encode.object [ ( "tock", Encode.int (Time.posixToMillis t) ) ]

        GameAction action ->
            Engine.msgEncoder action


msgDecoder : Decoder Msg
msgDecoder =
    Decode.oneOf
        [ Decode.map Tock (Decode.field "tock" Decode.int |> Decode.map Time.millisToPosix)
        , Decode.map GameAction Engine.msgDecoder
        ]


modelEncoder : Model -> Value
modelEncoder model =
    Encode.object
        [ ( "state", Engine.stateEncoder model.state )
        , ( "playerId", Encode.string model.playerId )
        , ( "time", Encode.int model.time )
        ]


modelDecoder : Decoder Model
modelDecoder =
    Decode.map3 Model
        (Decode.field "state" Engine.stateDecoder)
        (Decode.field "playerId" Decode.string)
        (Decode.field "time" Decode.int)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 10000 Tock


type alias Model =
    { state : State
    , playerId : PlayerId
    , time : Int
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
    ( Model state playerId 0, msg, Cmd.none )


type Msg
    = GameAction Engine.InternalMsg
    | Tock Time.Posix


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


css : Game.AssetMapping -> Maybe String
css asset =
    asset "test.css"


view : Game.AssetMapping -> Model -> Html Msg
view asset model =
    div []
        [ text "Tic Tac Toe"
        , img [ src (Maybe.withDefault "" (asset "test.svg")) ] []
        , text (String.fromInt model.time)
        , text (descPlayer model.playerId model.state.players)
        , text (Maybe.map (\winner -> " - " ++ showPlayer winner ++ " wins! 🎊") model.state.winner |> Maybe.withDefault "")
        , div []
            (model.state.tiles
                |> List.indexedMap showTile
                |> Engine.chunk 3
                |> List.map (div [])
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tock t ->
            ( { model | time = model.time + 1 }, Cmd.none )

        GameAction action ->
            let
                ( state_, gameCmd ) =
                    Engine.update (Game.PlayerMsg model.playerId action) model.state
            in
            ( { model | state = state_ }, Cmd.none )


setGameState : State -> Model -> Model
setGameState state model =
    { model | state = state }
