module Game.Wordle.Helpers exposing (..)

import Game.Wordle.Types exposing (AnnotatedGuess, Annotation(..))
import List.Extra as ListEx


type Status
    = Won
    | Lost
    | Playing


annotateGuess : String -> String -> Result String AnnotatedGuess
annotateGuess target guess =
    if String.length target /= String.length guess then
        Err ("invalid guess len " ++ guess)

    else
        ListEx.zip (String.toList target) (String.toList guess)
            |> List.map
                (\( tc, gc ) ->
                    if tc == gc then
                        ( gc, Green )

                    else if String.contains (String.fromChar gc) target then
                        -- TODO: Yellow only shows up once, right?
                        ( gc, Yellow )

                    else
                        ( gc, Grey )
                )
            |> Ok


append : a -> List a -> List a
append x l =
    List.reverse (x :: List.reverse l)


getStatus : List AnnotatedGuess -> Status
getStatus guesses =
    if List.any (List.all (\( l, c ) -> c == Green)) guesses then
        Won

    else if List.length guesses == 5 then
        Lost

    else
        Playing
