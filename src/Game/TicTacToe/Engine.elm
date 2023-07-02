module Game.TicTacToe.Engine exposing (InternalMsg(..), Player(..), State, Tile(..), engine, init, msgDecoder, msgEncoder, stateDecoder, stateEncoder, subscriptions, update)

import Dict exposing (Dict)
import Game exposing (GameMsg, PlayerId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as ListEx


engine : Game.Engine State InternalMsg
engine =
    { init = init
    , update = update
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
    , players : Dict PlayerId Player
    }


init : ( State, Cmd InternalMsg )
init =
    ( { turn = X, tiles = List.repeat 9 Open, winner = Nothing, players = Dict.empty }, Cmd.none )


type alias Msg =
    GameMsg InternalMsg


type InternalMsg
    = JoinGame
    | Claim Int


other : Player -> Player
other p =
    case p of
        X ->
            O

        O ->
            X


updateInternal : InternalMsg -> Maybe PlayerId -> State -> ( State, Cmd InternalMsg )
updateInternal msg maybePlayerId state =
    case msg of
        JoinGame ->
            let
                newPlayers =
                    case maybePlayerId of
                        Just playerId ->
                            case Dict.get playerId state.players of
                                Nothing ->
                                    case Dict.size state.players of
                                        0 ->
                                            Dict.insert playerId X state.players

                                        1 ->
                                            Dict.insert playerId O state.players

                                        _ ->
                                            state.players

                                _ ->
                                    state.players

                        _ ->
                            state.players
            in
            ( { state | players = newPlayers }, Cmd.none )

        Claim x ->
            case maybePlayerId of
                Just playerId ->
                    case Dict.get playerId state.players of
                        Just player ->
                            if state.turn == player then
                                case ListEx.getAt x state.tiles of
                                    Just Open ->
                                        let
                                            tiles =
                                                ListEx.setAt x (Taken player) state.tiles

                                            -- TODO: Check for winner
                                        in
                                        ( { state | tiles = tiles, turn = other state.turn }, Cmd.none )

                                    _ ->
                                        ( state, Cmd.none )

                            else
                                ( state, Cmd.none )

                        _ ->
                            ( state, Cmd.none )

                Nothing ->
                    ( state, Cmd.none )


update : Msg -> State -> ( State, Cmd InternalMsg )
update msg model =
    case msg of
        Game.PlayerMsg playerId internalMsg ->
            updateInternal internalMsg (Just playerId) model

        Game.SystemMsg internalMsg ->
            updateInternal internalMsg Nothing model

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
        , ( "players", Encode.dict identity encodePlayer state.players )
        ]


stateDecoder : Decoder State
stateDecoder =
    Decode.map4 State
        (Decode.field "turn" decodePlayer)
        (Decode.field "tiles" (Decode.list decodeTile))
        (Decode.field "winner" (Decode.nullable decodePlayer))
        (Decode.field "players" (Decode.dict decodePlayer))


msgEncoder : InternalMsg -> Value
msgEncoder msg =
    case msg of
        JoinGame ->
            Encode.string "join-game"

        Claim x ->
            Encode.object [ ( "claim", Encode.int x ) ]


decodeMatch : x -> String -> Decoder x
decodeMatch x str =
    Decode.string
        |> Decode.andThen
            (\s ->
                if s == str then
                    Decode.succeed x

                else
                    Decode.fail ("does not match " ++ str)
            )


msgDecoder : Decoder InternalMsg
msgDecoder =
    Decode.oneOf
        [ decodeMatch JoinGame "join-game"
        , Decode.map Claim (Decode.field "claim" Decode.int)
        ]
