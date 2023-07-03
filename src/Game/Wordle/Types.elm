module Game.Wordle.Types exposing (..)

import Dict exposing (Dict)
import Game exposing (PlayerId, decodePlayerId, encodePlayerId)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- [generator-start]


type EngineMsg
    = JoinGame
    | SetTarget String
    | Guess String


type ViewMsg
    = SetGuess String


type alias State =
    { player : Maybe PlayerId
    , guesses : List AnnotatedGuess
    , target : Maybe String
    }


type alias Model =
    { playerId : PlayerId
    , guess : String
    }


type alias AnnotatedGuess =
    List ( Char, Annotation )


type Annotation
    = Green
    | Yellow
    | Grey



-- [generator-generated-start] -- DO NOT MODIFY or remove this line
decodeAnnotatedGuess =
   Decode.list decodeTuple_Char_Annotation_

decodeAnnotation =
   let
      recover x =
         case x of
            "Green"->
               Decode.succeed Green
            "Yellow"->
               Decode.succeed Yellow
            "Grey"->
               Decode.succeed Grey
            other->
               Decode.fail <| "Unknown constructor for type Annotation: " ++ other
   in
      Decode.string |> Decode.andThen recover

decodeEngineMsg =
   Decode.field "Constructor" Decode.string |> Decode.andThen decodeEngineMsgHelp

decodeEngineMsgHelp constructor =
   case constructor of
      "JoinGame" ->
         Decode.succeed JoinGame
      "SetTarget" ->
         Decode.map
            SetTarget
               ( Decode.field "A1" Decode.string )
      "Guess" ->
         Decode.map
            Guess
               ( Decode.field "A1" Decode.string )
      other->
         Decode.fail <| "Unknown constructor for type EngineMsg: " ++ other

decodeModel =
   Decode.map2
      Model
         ( Decode.field "playerId" decodePlayerId )
         ( Decode.field "guess" Decode.string )

decodeState =
   Decode.map3
      State
         ( Decode.field "player" (Decode.maybe decodePlayerId) )
         ( Decode.field "guesses" (Decode.list decodeAnnotatedGuess) )
         ( Decode.field "target" (Decode.maybe Decode.string) )

decodeTuple_Char_Annotation_ =
   Decode.map2
      (\a1 a2 -> (a1, a2))
         ( Decode.field "A1" decodeChar )
         ( Decode.field "A2" decodeAnnotation )

decodeViewMsg =
   Decode.map SetGuess Decode.string

encodeAnnotatedGuess a =
   (Encode.list encodeTuple_Char_Annotation_) a

encodeAnnotation a =
   case a of
      Green ->
         Encode.string "Green"
      Yellow ->
         Encode.string "Yellow"
      Grey ->
         Encode.string "Grey"

encodeEngineMsg a =
   case a of
      JoinGame ->
         Encode.object
            [ ("Constructor", Encode.string "JoinGame")
            ]
      SetTarget a1->
         Encode.object
            [ ("Constructor", Encode.string "SetTarget")
            , ("A1", Encode.string a1)
            ]
      Guess a1->
         Encode.object
            [ ("Constructor", Encode.string "Guess")
            , ("A1", Encode.string a1)
            ]

encodeMaybePlayerId a =
   case a of
      Just b->
         encodePlayerId b
      Nothing->
         Encode.null

encodeMaybeString a =
   case a of
      Just b->
         Encode.string b
      Nothing->
         Encode.null

encodeModel a =
   Encode.object
      [ ("playerId", encodePlayerId a.playerId)
      , ("guess", Encode.string a.guess)
      ]

encodeState a =
   Encode.object
      [ ("player", encodeMaybePlayerId a.player)
      , ("guesses", (Encode.list encodeAnnotatedGuess) a.guesses)
      , ("target", encodeMaybeString a.target)
      ]

encodeTuple_Char_Annotation_ (a1, a2) =
   Encode.object
      [ ("A1", encodeChar a1)
      , ("A2", encodeAnnotation a2)
      ]

encodeViewMsg (SetGuess a1) =
   Encode.string a1 
-- [generator-end]

encodeChar : Char -> Value
encodeChar c =
    c |> String.fromChar |> Encode.string


decodeChar : Decoder Char
decodeChar =
    Decode.string
        |> Decode.andThen
            (\s ->
                case String.uncons s of
                    Just ( c, rest ) ->
                        Decode.succeed c

                    Nothing ->
                        Decode.fail "empty string"
            )
