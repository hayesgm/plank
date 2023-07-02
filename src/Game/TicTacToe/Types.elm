module Game.TicTacToe.Types exposing (..)

import Dict exposing (Dict)
import Game exposing (PlayerId, decodePlayerId, encodePlayerId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Time exposing (Posix)



-- [generator-start]


type EngineMsg
    = JoinGame
    | Claim Int
    | Tick Posix


type ViewMsg
    = EngineMsg EngineMsg
    | Tock Posix


type alias Model =
    { state : State
    , playerId : PlayerId
    , time : Int
    }


type alias State =
    { turn : Player
    , tiles : List Tile
    , winner : Maybe Player
    , players : Dict PlayerId Player
    }


type Player
    = X
    | O


type Tile
    = Open
    | Taken Player



-- [generator-generated-start] -- DO NOT MODIFY or remove this line
decodeDictPlayerIdPlayer =
   let
      decodeDictPlayerIdPlayerTuple =
         Decode.map2
            (\a1 a2 -> (a1, a2))
               ( Decode.field "A1" decodePlayerId )
               ( Decode.field "A2" decodePlayer )
   in
      Decode.map Dict.fromList (Decode.list decodeDictPlayerIdPlayerTuple)

decodeEngineMsg =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeEngineMsgHelp

decodeEngineMsgHelp constructor =
   case constructor of
      "JoinGame" ->
         Decode.succeed JoinGame
      "Claim" ->
         Decode.map
            Claim
               ( Decode.field "A1" Decode.int )
      "Tick" ->
         Decode.map
            Tick
               ( Decode.field "A1" decodePosix )
      other->
         Decode.fail <| "Unknown constructor for type EngineMsg: " ++ other

decodeModel =
   Decode.map3
      Model
         ( Decode.field "state" decodeState )
         ( Decode.field "playerId" decodePlayerId )
         ( Decode.field "time" Decode.int )

decodePlayer =
   let
      recover x =
         case x of
            "X"->
               Decode.succeed X
            "O"->
               Decode.succeed O
            other->
               Decode.fail <| "Unknown constructor for type Player: " ++ other
   in
      Decode.string |> Decode.andThen recover

decodeState =
   Decode.map4
      State
         ( Decode.field "turn" decodePlayer )
         ( Decode.field "tiles" (Decode.list decodeTile) )
         ( Decode.field "winner" (Decode.maybe decodePlayer) )
         ( Decode.field "players" decodeDictPlayerIdPlayer )

decodeTile =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeTileHelp

decodeTileHelp constructor =
   case constructor of
      "Open" ->
         Decode.succeed Open
      "Taken" ->
         Decode.map
            Taken
               ( Decode.field "A1" decodePlayer )
      other->
         Decode.fail <| "Unknown constructor for type Tile: " ++ other

decodeViewMsg =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeViewMsgHelp

decodeViewMsgHelp constructor =
   case constructor of
      "EngineMsg" ->
         Decode.map
            EngineMsg
               ( Decode.field "A1" decodeEngineMsg )
      "Tock" ->
         Decode.map
            Tock
               ( Decode.field "A1" decodePosix )
      other->
         Decode.fail <| "Unknown constructor for type ViewMsg: " ++ other

encodeDictPlayerIdPlayer a =
   let
      encodeDictPlayerIdPlayerTuple (a1,a2) =
         Encode.object
            [ ("A1", encodePlayerId a1)
            , ("A2", encodePlayer a2) ]
   in
      (Encode.list encodeDictPlayerIdPlayerTuple) (Dict.toList a)

encodeEngineMsg a =
   case a of
      JoinGame ->
         Encode.object
            [ ("Constructor", Encode.string "JoinGame")
            ]
      Claim a1->
         Encode.object
            [ ("Constructor", Encode.string "Claim")
            , ("A1", Encode.int a1)
            ]
      Tick a1->
         Encode.object
            [ ("Constructor", Encode.string "Tick")
            , ("A1", encodePosix a1)
            ]

encodeMaybePlayer a =
   case a of
      Just b->
         encodePlayer b
      Nothing->
         Encode.null

encodeModel a =
   Encode.object
      [ ("state", encodeState a.state)
      , ("playerId", encodePlayerId a.playerId)
      , ("time", Encode.int a.time)
      ]

encodePlayer a =
   case a of
      X ->
         Encode.string "X"
      O ->
         Encode.string "O"

encodeState a =
   Encode.object
      [ ("turn", encodePlayer a.turn)
      , ("tiles", (Encode.list encodeTile) a.tiles)
      , ("winner", encodeMaybePlayer a.winner)
      , ("players", encodeDictPlayerIdPlayer a.players)
      ]

encodeTile a =
   case a of
      Open ->
         Encode.object
            [ ("Constructor", Encode.string "Open")
            ]
      Taken a1->
         Encode.object
            [ ("Constructor", Encode.string "Taken")
            , ("A1", encodePlayer a1)
            ]

encodeViewMsg a =
   case a of
      EngineMsg a1->
         Encode.object
            [ ("Constructor", Encode.string "EngineMsg")
            , ("A1", encodeEngineMsg a1)
            ]
      Tock a1->
         Encode.object
            [ ("Constructor", Encode.string "Tock")
            , ("A1", encodePosix a1)
            ] 
-- [generator-end]

encodePosix : Posix -> Value
encodePosix =
    Time.posixToMillis >> Encode.int


decodePosix : Decoder Posix
decodePosix =
    Decode.int |> Decode.map Time.millisToPosix
