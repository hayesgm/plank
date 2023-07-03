module Game.Wordle.Game exposing (game)

import Browser
import Dict exposing (Dict)
import Game exposing (GameMsg(..), InboundMsg(..), PlayerId)
import Game.Wordle.Engine as Engine exposing (engine)
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
    , modelEncoder = Types.encodeModel
    , modelDecoder = Types.decodeModel
    , engine = engine
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
    ( Model playerId "", msg, Cmd.none )


subscriptions : Model -> State -> Sub (GameMsg EngineMsg ViewMsg)
subscriptions model state =
    Sub.none


view : Game.AssetMapping -> Model -> State -> Html (GameMsg EngineMsg ViewMsg)
view asset model state =
    div []
        [ text "Wordle"
        , img [ src (Maybe.withDefault "" (asset "wordle.svg")) ] []
        , div []
            [ text
                (case getStatus state.guesses of
                    Won ->
                        "You won!"

                    Lost ->
                        "You lost!"

                    Playing ->
                        ""
                )
            ]
        , text (descPlayer model.playerId state.player)
        , div [] (List.map showGuess state.guesses)
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


update : ViewMsg -> Model -> State -> ( Model, Cmd (GameMsg EngineMsg ViewMsg) )
update msg model state =
    case msg of
        SetGuess guess ->
            ( { model | guess = guess }, Cmd.none )


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
