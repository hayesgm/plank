module Game.TicTacToe.Game exposing (game)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), InboundMsg(..), PlayerId)
import Game.TicTacToe.Engine as Engine exposing (update)
import Game.TicTacToe.Helpers as Helpers exposing (chunk)
import Game.TicTacToe.Types as Types exposing (EngineMsg(..), Model, Player(..), State, Tile(..), ViewMsg(..))
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Time


game : Game.Game Model State EngineMsg ViewMsg
game =
    { view = view
    , css = css
    , init = init
    , update = update
    , subscriptions = subscriptions
    , gameMsgEncoder = Types.encodeViewMsg
    , gameMsgDecoder = Types.decodeViewMsg
    , engineMsgEncoder = Types.encodeEngineMsg
    , engineMsgDecoder = Types.decodeEngineMsg
    , modelEncoder = Types.encodeModel
    , modelDecoder = Types.decodeModel
    , stateEncoder = Types.encodeState
    , stateDecoder = Types.decodeState
    , setGameState = setGameState
    }


init : PlayerId -> State -> ( Model, Maybe (GameMsg EngineMsg ViewMsg), Cmd (GameMsg EngineMsg ViewMsg) )
init playerId state =
    let
        msg =
            if Dict.size state.players < 2 then
                Just (ForEngine Types.JoinGame)

            else
                Nothing
    in
    ( Model state playerId 0, msg, Cmd.none )


subscriptions : Model -> Sub (GameMsg EngineMsg ViewMsg)
subscriptions model =
    Time.every 10000 (ForSelf << Tock)


view : Game.AssetMapping -> Model -> Html (GameMsg EngineMsg ViewMsg)
view asset model =
    div []
        [ text "Tic Tac Toe"
        , img [ src (Maybe.withDefault "" (asset "tic-tac-toe.svg")) ] []
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
    asset "tic-tac-toe.css"


update : InboundMsg EngineMsg ViewMsg -> Model -> ( Model, Cmd (GameMsg EngineMsg ViewMsg) )
update msg model =
    case msg of
        GameMsg (Tock t) ->
            ( { model | time = model.time + 1 }, Cmd.none )

        EngineMsg engineMsg ->
            let
                ( state_, gameCmd ) =
                    Engine.update engineMsg model.state
            in
            ( { model | state = state_ }, Cmd.none )


setGameState : State -> Model -> Model
setGameState state model =
    { model | state = state }


showTile : Int -> Tile -> Html (GameMsg EngineMsg ViewMsg)
showTile pos tile =
    case tile of
        Open ->
            span [ onClick (ForEngine (Types.Claim pos)) ] [ text " [ ] " ]

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
