module Game.Utils.Deck exposing (..)

import Random exposing (Generator)
import Random.List exposing (shuffle)


type Deck a
    = Deck (List a)


type ShuffledDeck a
    = ShuffledDeck (Deck a)


new : List a -> Deck a
new list =
    Deck list


random : Deck a -> Generator (ShuffledDeck a)
random (Deck list) =
    Random.map (ShuffledDeck << Deck) (shuffle list)


draw : ShuffledDeck a -> ( Maybe a, ShuffledDeck a )
draw (ShuffledDeck (Deck deck)) =
    case deck of
        [] ->
            ( Nothing, ShuffledDeck (Deck []) )

        top :: rest ->
            ( Just top, ShuffledDeck (Deck rest) )


append : a -> ShuffledDeck a -> ShuffledDeck a
append card (ShuffledDeck (Deck deck)) =
    ShuffledDeck (Deck (List.append deck [ card ]))


appendTop : a -> ShuffledDeck a -> ShuffledDeck a
appendTop card (ShuffledDeck (Deck deck)) =
    ShuffledDeck (Deck (card :: deck))


length : ShuffledDeck a -> Int
length (ShuffledDeck (Deck deck)) =
    List.length deck


map : (a -> b) -> ShuffledDeck a -> List b
map func (ShuffledDeck (Deck list)) =
    List.map func list


filter : (a -> Bool) -> ShuffledDeck a -> ShuffledDeck a
filter func (ShuffledDeck (Deck list)) =
    ShuffledDeck (Deck (List.filter func list))


foldl : (a -> b -> b) -> b -> ShuffledDeck a -> b
foldl func initialValue (ShuffledDeck (Deck list)) =
    List.foldl func initialValue list


foldr : (a -> b -> b) -> b -> ShuffledDeck a -> b
foldr func initialValue (ShuffledDeck (Deck list)) =
    List.foldr func initialValue list


take : Int -> ShuffledDeck a -> ShuffledDeck a
take amount (ShuffledDeck (Deck list)) =
    ShuffledDeck (Deck (List.take amount list))
