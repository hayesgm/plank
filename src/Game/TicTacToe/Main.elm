module Game.TicTacToe.Main exposing (State, InternalMsg(..), init, main, subscriptions, update, view)

import Game exposing (GameMsg)
import Browser
import Helper
import Html exposing (Html, div, text)
import Input


main : Program () State Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : State -> Sub Msg
subscriptions model =
    Sub.none


type alias State =
    { ticks : Int
    , pings : List Int
    }


init : ( State, Cmd Msg )
init =
    ( { ticks = 0, pings = [] }, Cmd.none )


type alias Msg = GameMsg InternalMsg

type InternalMsg
    = Ping Int


view : State -> Html Msg
view model =
    div [] []


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        Game.PlayerMsg (Ping x) ->
            ( { model | pings = x :: model.pings }, Cmd.none )
        
        Game.Tick ->
            ( { model | ticks = model.ticks + 1 }, Cmd.none )
