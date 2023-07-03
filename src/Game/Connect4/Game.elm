module Game.Connect4.Game exposing (game)

import Dict exposing (Dict)
import Game exposing (GameMsg(..), InboundMsg(..), PlayerId)
import Game.Connect4.Engine exposing (engine)
import Game.Connect4.Helpers exposing (cols)
import Game.Connect4.Types as Types exposing (EngineMsg(..), Model, Player(..), State, Tile(..), ViewMsg(..))
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
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
    , modelEncoder = Types.encodeModel
    , modelDecoder = Types.decodeModel
    , engine = engine
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
    ( Model playerId 0, msg, Cmd.none )


subscriptions : Model -> State -> Sub (GameMsg EngineMsg ViewMsg)
subscriptions _ _ =
    Time.every 10000 (ForSelf << Tock)


view : Game.AssetMapping -> Model -> State -> Html (GameMsg EngineMsg ViewMsg)
view _ model state =
    div []
        [ text "Connect 4"
        , text (String.fromInt model.time)
        , text (descPlayer model.playerId state.players)
        , text (Maybe.map (\winner -> " - " ++ showPlayer winner ++ " wins! 🎊") state.winner |> Maybe.withDefault "")
        , div []
            (cols state.tiles
                |> List.indexedMap showCol
            )
        ]


css : Game.AssetMapping -> Maybe String
css asset =
    asset "connect4.css"


update : ViewMsg -> Model -> State -> ( Model, Cmd (GameMsg EngineMsg ViewMsg) )
update msg model _ =
    case msg of
        Tock _ ->
            ( { model | time = model.time + 1 }, Cmd.none )


showTile : Tile -> Html (GameMsg EngineMsg ViewMsg)
showTile tile =
    case tile of
        Open ->
            span [] [ text " [ ] " ]

        Taken p ->
            span [] [ text (String.concat [ " [", showPlayer p, "] " ]) ]


showCol : Int -> List Tile -> Html (GameMsg EngineMsg ViewMsg)
showCol col tiles =
    let
        maybeAvailablePos : List Tile -> Maybe Int
        maybeAvailablePos tiles_ =
            List.reverse tiles_
                |> List.head
                |> Maybe.andThen
                    (\last ->
                        let
                            row =
                                List.length tiles_ - 1
                        in
                        case last of
                            Open ->
                                Just (col + (7 * row))

                            _ ->
                                maybeAvailablePos (List.take row tiles_)
                    )

        msg =
            case maybeAvailablePos tiles of
                Just pos ->
                    [ onClick (ForEngine (Types.Claim pos)) ]

                Nothing ->
                    []
    in
    div (class "col" :: msg) (List.map showTile tiles)


showPlayer : Player -> String
showPlayer p =
    case p of
        X ->
            "🔴"

        O ->
            "🟡"


descPlayer : PlayerId -> Dict PlayerId Player -> String
descPlayer p players =
    case Dict.get p players of
        Just player ->
            "Playing as " ++ showPlayer player

        _ ->
            "Spectating"
