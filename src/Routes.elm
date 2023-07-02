module Routes exposing (..)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, fragment, int, map, oneOf, s, string)


type Route
    = Home
    | Game String
    | NotFound


getUrl : Route -> String
getUrl r =
    case r of
        Home ->
            "/"

        Game gameId ->
            "/#" ++ gameId

        NotFound ->
            "/"


getRoute : Url -> Route
getRoute url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map
            (\f ->
                case f of
                    Nothing ->
                        Home

                    Just gameId ->
                        Game gameId
            )
            (fragment identity)
        ]
