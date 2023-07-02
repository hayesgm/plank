module Game.TicTacToe.Engine exposing (InternalMsg(..), Player(..), State, Tile(..), chunk, engine, init, msgDecoder, msgEncoder, stateDecoder, stateEncoder, subscriptions, update)

import Dict exposing (Dict)
import Game exposing (GameMsg, PlayerId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import List.Extra as ListEx
import Time


engine : Game.Engine State InternalMsg
engine =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , msgEncoder = msgEncoder
    , msgDecoder = msgDecoder
    , stateEncoder = stateEncoder
    , stateDecoder = stateDecoder
    , publicStateEncoder = stateEncoder
    , publicStateDecoder = stateDecoder
    }


subscriptions : State -> Sub InternalMsg
subscriptions model =
    Time.every 10000 Tock


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
    | Tock Time.Posix


other : Player -> Player
other p =
    case p of
        X ->
            O

        O ->
            X


claimedByPlayer : Player -> Tile -> Bool
claimedByPlayer player tile =
    case tile of
        Taken player_ ->
            player == player_

        _ ->
            False


checkWinner : List Tile -> Maybe Player
checkWinner tiles =
    let
        sets =
            rows tiles
                |> List.append (cols tiles)
                |> List.append (diags tiles)

        isWinner : Player -> Bool
        isWinner player =
            List.any (List.all (claimedByPlayer player)) sets
    in
    case ( isWinner X, isWinner O ) of
        ( True, _ ) ->
            Just X

        ( _, True ) ->
            Just O

        _ ->
            Nothing


updateInternal : InternalMsg -> Maybe PlayerId -> State -> ( State, Cmd InternalMsg )
updateInternal msg maybePlayerId state =
    case msg of
        Tock t ->
            ( state, Cmd.none )

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
            case ( maybePlayerId, state.winner ) of
                ( Just playerId, Nothing ) ->
                    case Dict.get playerId state.players of
                        Just player ->
                            if state.turn == player then
                                case ListEx.getAt x state.tiles of
                                    Just Open ->
                                        let
                                            tiles =
                                                ListEx.setAt x (Taken player) state.tiles

                                            maybeWinner =
                                                checkWinner tiles
                                        in
                                        ( { state | tiles = tiles, turn = other state.turn, winner = maybeWinner }, Cmd.none )

                                    _ ->
                                        ( state, Cmd.none )

                            else
                                ( state, Cmd.none )

                        _ ->
                            ( state, Cmd.none )

                _ ->
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
        Tock t ->
            Encode.object [ ( "tock", Encode.int (Time.posixToMillis t) ) ]

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
        , Decode.map Tock (Decode.field "tock" Decode.int |> Decode.map Time.millisToPosix)
        ]


chunk : Int -> List a -> List (List a)
chunk n l =
    if List.isEmpty l then
        []

    else
        List.take n l :: chunk n (List.drop n l)


rows : List Tile -> List (List Tile)
rows tiles =
    chunk 3 tiles


remap : List a -> List Int -> List a
remap l mapping =
    mapping
        |> List.foldl
            (\el acc ->
                case ListEx.getAt el l of
                    Just x ->
                        x :: acc

                    _ ->
                        acc
            )
            []
        |> List.reverse


cols : List Tile -> List (List Tile)
cols tiles =
    remap tiles [ 0, 3, 6, 1, 4, 7, 2, 5, 8 ]
        |> chunk 3


diags : List Tile -> List (List Tile)
diags tiles =
    remap tiles [ 0, 4, 8, 2, 4, 6 ]
        |> chunk 3
