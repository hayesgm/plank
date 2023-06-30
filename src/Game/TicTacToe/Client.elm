module Game.TicTacToe.Client exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Game.TicTacToe.Main exposing (State, init)

import Browser
import Html exposing (Html, div, text)
import Html exposing (Html, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Game exposing (GameMsg(..))

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
    { state : State
    , name : String
    }


init : ( Model, Cmd Msg )
init =
    let ( state, _ ) = Game.TicTacToe.Main.init in
      ( Model state "tic tac toe", Cmd.none )


type Msg
    = Rename String


view : Model -> Html Msg
view model =
    div []
        [ text model.name
        , input
          [ type_ "text"
          , onInput Rename
          , value model.name
          ]
          []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rename str ->
            ( { model | name = str }, Cmd.none )
