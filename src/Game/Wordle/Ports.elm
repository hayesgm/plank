port module Game.Wordle.Ports exposing (wordleFetch, wordleGot)


port wordleFetch : () -> Cmd msg


port wordleGot : (String -> msg) -> Sub msg
