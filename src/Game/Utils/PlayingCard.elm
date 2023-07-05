module Game.Utils.PlayingCard exposing (..)


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Value
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


type Card
    = Card Suit Value


stringToMaybeSuit : String -> Maybe Suit
stringToMaybeSuit string =
    case String.toLower string of
        "clubs" ->
            Just Clubs

        "diamonds" ->
            Just Diamonds

        "hearts" ->
            Just Hearts

        "spades" ->
            Just Spades

        _ ->
            Nothing


numberToMaybeValue : Int -> Maybe Value
numberToMaybeValue number =
    case number of
        1 ->
            Just Ace

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        _ ->
            Nothing
