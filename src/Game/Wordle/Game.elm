module Game.Wordle.Game exposing (game)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), InboundMsg(..), PlayerId)
import Game.Wordle.Engine as Engine exposing (update)
import Game.Wordle.Helpers as Helpers exposing (Status(..), getStatus)
import Game.Wordle.Types as Types exposing (AnnotatedGuess, Annotation(..), EngineMsg(..), Model, State, ViewMsg(..))
import Html exposing (Html, button, div, img, input, span, text)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)


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
            case state.player of
                Nothing ->
                    Just (ForEngine Types.JoinGame)

                _ ->
                    Nothing
    in
    ( Model state playerId "", msg, Cmd.none )


subscriptions : Model -> Sub (GameMsg EngineMsg ViewMsg)
subscriptions model =
    Sub.none


view : Game.AssetMapping -> Model -> Html (GameMsg EngineMsg ViewMsg)
view asset model =
    div []
        [ text "Wordle"
        , img [ src (Maybe.withDefault "" (asset "wordle.svg")) ] []
        , div []
            [ text
                (case getStatus model.state.guesses of
                    Won ->
                        "You won!"

                    Lost ->
                        "You lost!"

                    Playing ->
                        ""
                )
            ]
        , text (descPlayer model.playerId model.state.player)
        , div [] (List.map showGuess model.state.guesses)
        , input [ placeholder "Guess", value model.guess, onInput (ForSelf << SetGuess) ] []
        , button [ onClick (ForEngine (Types.Guess model.guess)) ] [ text "Guess" ]
        ]


css : Game.AssetMapping -> Maybe String
css asset =
    asset "wordle.css"


showGuess : AnnotatedGuess -> Html (GameMsg EngineMsg ViewMsg)
showGuess guess =
    div []
        (List.map
            (\( letter, annotation ) ->
                let
                    colorClass =
                        case annotation of
                            Green ->
                                "green"

                            Yellow ->
                                "yellow"

                            Grey ->
                                "grey"
                in
                span [ class "square", class colorClass ] [ text (String.fromChar letter) ]
            )
            guess
        )


update : InboundMsg EngineMsg ViewMsg -> Model -> ( Model, Cmd (GameMsg EngineMsg ViewMsg) )
update msg model =
    case msg of
        GameMsg (SetGuess guess) ->
            ( { model | guess = guess }, Cmd.none )

        EngineMsg engineMsg ->
            let
                ( state_, gameCmd ) =
                    Engine.update engineMsg model.state
            in
            ( { model | state = state_ }, Cmd.none )


setGameState : State -> Model -> Model
setGameState state model =
    { model | state = state }


descPlayer : PlayerId -> Maybe PlayerId -> String
descPlayer p gamePlayer =
    case gamePlayer of
        Just player ->
            if p == player then
                "Playing"

            else
                "Spectating"

        _ ->
            "Spectating"
