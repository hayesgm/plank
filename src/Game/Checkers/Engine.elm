module Game.Checkers.Engine exposing (InternalMsg(..), Player(..), State, Tile(..), engine, init, msgDecoder, msgEncoder, stateDecoder, stateEncoder, subscriptions, update)

import Game exposing (GameMsg)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as ListEx


engine : Game.Engine State InternalMsg
engine =
    { init = init
    , update = updateInternal
    , subscriptions = subscriptions
    , msgEncoder = msgEncoder
    , msgDecoder = msgDecoder
    , stateEncoder = stateEncoder
    , stateDecoder = stateDecoder
    }


subscriptions : State -> Sub InternalMsg
subscriptions model =
    Sub.none


type Player
    = X
    | O


type Tile
    = Open
    | Taken Player


type alias State =
    { turn : Player
    , tiles : List Tile
    , winner : Maybe Player
    }


init : ( State, Cmd InternalMsg )
init =
    ( { turn = X, tiles = List.repeat 9 Open, winner = Nothing }, Cmd.none )


type alias Msg =
    GameMsg InternalMsg


type InternalMsg
    = Claim Int


other : Player -> Player
other p =
    case p of
        X ->
            O

        O ->
            X


updateInternal : InternalMsg -> State -> ( State, Cmd InternalMsg )
updateInternal msg state =
    case msg of
        Claim x ->
            case ListEx.getAt x state.tiles of
                Just Open ->
                    let
                        tiles =
                            ListEx.setAt x (Taken state.turn) state.tiles

                        -- TODO: Check for winner
                    in
                    ( { state | tiles = tiles, turn = other state.turn }, Cmd.none )

                _ ->
                    ( state, Cmd.none )


update : Msg -> State -> ( State, Cmd InternalMsg )
update msg model =
    case msg of
        Game.PlayerMsg pm ->
            updateInternal pm model

        Game.Tick ->
            ( model, Cmd.none )


encodePlayer : Player -> Value
encodePlayer p =
    case p of
        X ->
            Encode.string "x"

        O ->
            Encode.string "o"


encodeTile : Tile -> Value
encodeTile t =
    case t of
        Open ->
            Encode.string "open"

        Taken player ->
            Encode.object [ ( "taken", encodePlayer player ) ]


decodePlayer : Decoder Player
decodePlayer =
    Decode.string
        |> Decode.andThen
            (\p ->
                case p of
                    "x" ->
                        Decode.succeed X

                    "o" ->
                        Decode.succeed O

                    _ ->
                        Decode.fail ("Unknown player " ++ p)
            )



-- TODO: Improve the open check here


decodeOpen : Decoder Tile
decodeOpen =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "open" ->
                        Decode.succeed Open

                    _ ->
                        Decode.fail "not open"
            )


decodeTile : Decoder Tile
decodeTile =
    Decode.oneOf
        [ Decode.map Taken (Decode.field "taken" decodePlayer)
        , decodeOpen
        ]


stateEncoder : State -> Value
stateEncoder state =
    Encode.object
        [ ( "turn", encodePlayer state.turn )
        , ( "tiles", Encode.list encodeTile state.tiles )
        , ( "winner", Maybe.map encodePlayer state.winner |> Maybe.withDefault Encode.null )
        ]


stateDecoder : Decoder State
stateDecoder =
    Decode.map3 State
        (Decode.field "turn" decodePlayer)
        (Decode.field "tiles" (Decode.list decodeTile))
        (Decode.field "winner" (Decode.nullable decodePlayer))


msgEncoder : InternalMsg -> Value
msgEncoder msg =
    case msg of
        Claim x ->
            Encode.object [ ( "claim", Encode.int x ) ]


msgDecoder : Decoder InternalMsg
msgDecoder =
    Decode.oneOf
        [ Decode.map Claim (Decode.field "claim" Decode.int)
        ]
