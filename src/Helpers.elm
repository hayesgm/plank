module Helpers exposing (..)

import List.Extra as ListEx

chunk : Int -> List a -> List (List a)
chunk n l =
    if List.isEmpty l then
        []

    else
        List.take n l :: chunk n (List.drop n l)


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
