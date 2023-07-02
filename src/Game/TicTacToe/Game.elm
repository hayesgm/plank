module Game.TicTacToe.Game exposing (game)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), PlayerId)
import Game.TicTacToe.Engine as Engine exposing (update)
import Game.TicTacToe.Helpers as Helpers exposing (chunk)
import Game.TicTacToe.Types as Types exposing (Model, Player(..), State, Tile(..), ViewMsg(..))
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Time


game : Game.Game Model State ViewMsg
game =
    { view = view
    , css = css
    , init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = Types.encodeViewMsg
    , msgDecoder = Types.decodeViewMsg
    , modelEncoder = Types.encodeModel
    , modelDecoder = Types.decodeModel
    , stateEncoder = Types.encodeState
    , stateDecoder = Types.decodeState
    , setGameState = setGameState
    }


init : PlayerId -> State -> ( Model, Maybe ViewMsg, Cmd ViewMsg )
init playerId state =
    let
        msg =
            if Dict.size state.players < 2 then
                Just (EngineMsg Types.JoinGame)

            else
                Nothing
    in
    ( Model state playerId 0, msg, Cmd.none )


subscriptions : Model -> Sub ViewMsg
subscriptions model =
    Time.every 10000 Tock


view : Game.AssetMapping -> Model -> Html ViewMsg
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
                |> chunk 3
                |> List.map (div [])
            )
        ]


css : Game.AssetMapping -> Maybe String
css asset =
    asset "test.css"


update : ViewMsg -> Model -> ( Model, Cmd ViewMsg )
update msg model =
    case msg of
        Tock t ->
            ( { model | time = model.time + 1 }, Cmd.none )

        EngineMsg action ->
            let
                ( state_, gameCmd ) =
                    Engine.update (Game.PlayerMsg model.playerId action) model.state
            in
            ( { model | state = state_ }, Cmd.none )


setGameState : State -> Model -> Model
setGameState state model =
    { model | state = state }


showTile : Int -> Tile -> Html ViewMsg
showTile pos tile =
    case tile of
        Open ->
            span [ onClick (EngineMsg (Types.Claim pos)) ] [ text " [ ] " ]

        Taken X ->
            span [] [ text " [X] " ]

        Taken O ->
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
