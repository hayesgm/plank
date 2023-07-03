module Game.Connect4.Helpers exposing (..)

import Game.Connect4.Types exposing (Player(..), Tile(..))
import Helpers exposing (chunk, remap)


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
        sets : List (List Tile)
        sets =
            List.concatMap (subsets 4) (rows tiles)
                |> List.append (List.concatMap (subsets 4) (cols tiles))
                |> List.append (List.concatMap (subsets 4) (diags tiles))

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


subsets : Int -> List a -> List (List a)
subsets n l =
    if List.length l < n then
        []

    else
        List.take n l :: subsets n (List.drop 1 l)


rows : List Tile -> List (List Tile)
rows tiles =
    chunk 7 tiles


cols : List Tile -> List (List Tile)
cols tiles =
    remap tiles
        [ 0
        , 7
        , 14
        , 21
        , 28
        , 35
        , 1
        , 8
        , 15
        , 22
        , 29
        , 36
        , 2
        , 9
        , 16
        , 23
        , 30
        , 37
        , 3
        , 10
        , 17
        , 24
        , 31
        , 38
        , 4
        , 11
        , 18
        , 25
        , 32
        , 39
        , 5
        , 12
        , 19
        , 26
        , 33
        , 40
        , 6
        , 13
        , 20
        , 27
        , 34
        , 41
        ]
        |> chunk 6


diags : List Tile -> List (List Tile)
diags tiles =
    [ remap tiles [ 14, 22, 30, 38 ]
    , remap tiles [ 7, 15, 23, 31, 39 ]
    , remap tiles [ 0, 8, 16, 24, 32, 40 ]
    , remap tiles [ 1, 9, 17, 25, 33, 41 ]
    , remap tiles [ 2, 10, 18, 26, 34 ]
    , remap tiles [ 3, 11, 19, 27 ]
    , remap tiles [ 3, 9, 15, 21 ]
    , remap tiles [ 4, 10, 16, 22, 28 ]
    , remap tiles [ 5, 11, 17, 23, 29, 35 ]
    , remap tiles [ 6, 12, 18, 24, 30, 36 ]
    , remap tiles [ 13, 19, 25, 31, 37 ]
    , remap tiles [ 20, 26, 32, 38 ]
    ]
