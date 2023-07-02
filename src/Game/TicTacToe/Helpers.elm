module Game.TicTacToe.Helpers exposing (..)

import Game.TicTacToe.Types exposing (Player(..), Tile(..))
import List.Extra as ListEx


otherPlayer : Player -> Player
otherPlayer p =
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
