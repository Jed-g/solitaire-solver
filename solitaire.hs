import System.Random
import Data.List

data Suit = Hearts | Clubs | Spades | Diamonds deriving (Show, Enum, Eq)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum, Eq)
type Card = (Pip, Suit)
type Deck = [Card]
pack :: Deck
pack = [(pip, suit) | pip <- enumFrom Ace, suit <- enumFrom Hearts]

sCard :: Card -> Card
sCard (pip, suit) = if pip == King then (Ace, suit) else (succ pip, suit)

pCard :: Card -> Card
pCard (pip, suit) = if pip == Ace then (King, suit) else (pred pip, suit)

isAce :: Card -> Bool
isAce (pip, _) = pip == Ace

isKing :: Card -> Bool
isKing (pip, _) = pip == King

shuffle :: Deck -> Deck
shuffle deck = [card | (card, _) <- sortBy cmp (zip deck (randoms (mkStdGen 1) :: [Int]))]
    where
        cmp (_, x) (_, y) = compare x y
    {-where
        timeOfDay = timeToTimeOfDay (utctDayTime getCurrentTime)
        currentHour = todHour timeOfDay
        currentMin = todMin timeOfDay
        currentSec = todSec timeOfDay
        seed = currentHour * 10000 + currentMin * 100 + currentSec-}

type Foundations = [Card]
type Columns = [[Card]]
type Reserve = [Card]
data Board = EOBoard Foundations Columns Reserve

appendixABoard :: Board
appendixABoard = EOBoard [] [[(Four, Spades), (King, Clubs), (Queen, Hearts), (Ace, Hearts), (Seven, Diamonds), (Ace, Clubs)],
    [(Seven, Hearts), (Six, Spades), (Five, Spades), (Three, Diamonds), (Queen, Diamonds), (Five, Diamonds)],
    [(Eight, Diamonds), (Five, Hearts), (Queen, Diamonds), (Seven, Spades), (Ten, Diamonds), (King, Hearts)],
    [(Queen, Spades), (Ten, Clubs), (Eight, Spades), (Seven, Clubs), (Six, Hearts), (Jack, Spades)],
    [(Four, Clubs), (Jack, Hearts), (King, Diamonds), (Ace, Diamonds), (Eight, Clubs), (Ace, Spades)],
    [(Jack, Clubs), (Six, Diamonds), (Ten, Hearts), (Three, Clubs), (Three, Hearts), (Two, Diamonds)],
    [(Ten, Spades), (Three, Spades), (Nine, Hearts), (Nine, Clubs), (Four, Diamonds), (Nine, Spades)],
    [(Eight, Hearts), (King, Spades), (Nine, Diamonds), (Four, Hearts), (Two, Spades), (Two, Clubs)]] [(Two, Hearts), (Six, Clubs), (Five, Clubs), (Jack, Diamonds)]

instance Show Board where
    show (EOBoard foundations columns reserve) = "EOBoard\nFoundations " ++ show foundations ++ "\nColumns\n" ++
        concat ["\t" ++ show column ++ "\n" | column <- columns] ++ "\nReserve " ++ show reserve
