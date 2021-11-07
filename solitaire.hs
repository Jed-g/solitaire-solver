import System.Random
import Data.List

data Suit = Hearts | Clubs | Spades | Diamonds deriving (Show, Enum, Eq)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq)
data Card = Card Pip Suit deriving Show
data Deck = Deck [Card] deriving Show

pack :: Deck
pack = Deck [(Card pip suit) | pip <- enumFrom Ace, suit <- enumFrom Hearts]

sCard :: Card -> Card
sCard (Card pip suit) = if pip == King then (Card Ace suit) else (Card (succ pip) suit)

pCard :: Card -> Card
pCard (Card pip suit) = if pip == Ace then (Card King suit) else (Card (pred pip) suit)

isAce :: Card -> Bool
isAce (Card pip _) = pip == Ace

isKing :: Card -> Bool
isKing (Card pip _) = pip == King

shuffle :: Deck -> Deck
shuffle (Deck listOfCards) = Deck [card | (card, _) <- sortBy cmp (zip listOfCards (randoms (mkStdGen 1) :: [Int]))]
    where
        cmp (_, x) (_, y) = compare x y
    {-where
        timeOfDay = timeToTimeOfDay (utctDayTime getCurrentTime)
        currentHour = todHour timeOfDay
        currentMin = todMin timeOfDay
        currentSec = todSec timeOfDay
        seed = currentHour * 10000 + currentMin * 100 + currentSec-}