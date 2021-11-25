{- Author: Jedrzej Golebiewski
COM2108 Functional Programming Grading Assignment -}

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

shuffle :: Int -> Deck -> Deck
shuffle seed deck = [card | (card, _) <- sortBy cmp (zip deck (randoms (mkStdGen seed) :: [Int]))]
    where
        cmp (_, x) (_, y) = compare x y

type Foundations = [Card]
type Columns = [[Card]]
type Reserve = [Card]
data Board = EOBoard Foundations Columns Reserve | SBoard Foundations Columns Hidden Stock deriving (Eq)

appendixABoard :: Board
appendixABoard = EOBoard [] [[(Ace, Clubs), (Seven, Diamonds), (Ace, Hearts), (Queen, Hearts), (King, Clubs), (Four, Spades)],
        [(Five, Diamonds), (Queen, Diamonds), (Three, Diamonds), (Five, Spades), (Six, Spades), (Seven, Hearts)],
        [(King, Hearts), (Ten, Diamonds), (Seven, Spades), (Queen, Diamonds), (Five, Hearts), (Eight, Diamonds)],
        [(Jack, Spades), (Six, Hearts), (Seven, Clubs), (Eight, Spades), (Ten, Clubs), (Queen, Spades)],
        [(Ace, Spades), (Eight, Clubs), (Ace, Diamonds), (King, Diamonds), (Jack, Hearts), (Four, Clubs)],
        [(Two, Diamonds), (Three, Hearts), (Three, Clubs), (Ten, Hearts), (Six, Diamonds), (Jack, Clubs)],
        [(Nine, Spades), (Four, Diamonds), (Nine, Clubs), (Nine, Hearts), (Three, Spades), (Ten, Spades)],
        [(Two, Clubs), (Two, Spades), (Four, Hearts), (Nine, Diamonds), (King, Spades), (Eight, Hearts)]] [(Two, Hearts),
        (Six, Clubs), (Five, Clubs), (Jack, Diamonds)]

instance Show Board where
    show (EOBoard foundations columns reserve) = "EOBoard\nFoundations " ++ show foundations ++ "\nColumns\n" ++
        concat ["\t" ++ show column ++ "\n" | column <- columns] ++ "Reserve " ++ show reserve

    show (SBoard foundations columns hidden stock) = "SBoard\nFoundations " ++ show foundations ++ "\nColumns\n" ++
        concat ["\t[" ++ generateInnerString column outerIndex ++ "]\n" | (column, outerIndex) <- zip columns [1..]] ++
        "Stock " ++ show (length stock `div` 10) ++ " Deals remaining"
        where
            {- 'hidden' is a list of (Int, Int) tuples, where the first Int is the index of the column where some cards are to be hidden
            and the second Int is the index in the column, where all the cards with indexes equal or higher are supposed to be hidden.

            A linebreak is placed every 6 cards in a column unless it is the end of the list so that there is nice formatting.-}

            generateInnerString col outIndex = 
                concat [(if null indexOfHiddenDef then show card else if snd(hidden !! (indexOfHiddenDef !! 0)) > innerIndex then show card else "<unknown>") ++
                (if innerIndex == length col then "" else ",") ++ (if innerIndex `mod` 6 == 0 && innerIndex < length col then "\n\t" else "") |
                (card, innerIndex) <- zip col [1..], let indexOfHiddenDef = findIndices (\(columnIndex, _) -> columnIndex == outIndex) hidden]

-- 'eODeal' takes in a 'seed' argument on the basis of which it generates a EOBoard.
eODeal :: Int -> Board
eODeal seed = EOBoard [] [[shuffledPack !! (outerIndex * 6 + innerIndex) | innerIndex <- [0..5]] | outerIndex <- [0..7]] [shuffledPack !! index | index <- [48..51]]
    where
        shuffledPack = shuffle seed pack

toFoundations :: Board -> Board
toFoundations (EOBoard foundations columns reserve) = EOBoard updatedFoundations updatedColumns updatedReserve
    where
        (updatedFoundations, updatedColumns, updatedReserve) = doAllPossibleMoves (foundations, columns, reserve)

{- Helper function for 'toFoundations'. It recursively calls itself until no cards can be moved. During each call, it creates a list with the cards that can be moved,
updates 'foundations' with these cards, updates 'columns' and 'reserve' by removing the cards that can be moved. If no cards can be moved, the function terminates.-} 
doAllPossibleMoves :: (Foundations, Columns, Reserve) -> (Foundations, Columns, Reserve)
doAllPossibleMoves (foundations, columns, reserve) = if null cardsThatCanBeMoved then (foundations, columns, reserve) else
    doAllPossibleMoves (updatedFoundations, updatedColumns, updatedReserve)
    where
        cardsThatCanBeMoved = [head column | column <- removeEmptyColumns columns, isAce (head column) || elem (pCard (head column)) foundations] ++
            [reserveCard | reserveCard <- reserve, isAce reserveCard || elem (pCard reserveCard) foundations]

        updatedFoundations = updateFoundations (foundations, cardsThatCanBeMoved)
        updatedColumns = updateColumns (columns, cardsThatCanBeMoved)
        updatedReserve = updateReserve (reserve, cardsThatCanBeMoved)

-- Helper functions for 'doAllPossibleMoves'. Functionality explained above.
updateFoundations :: (Foundations, Deck) -> Foundations
updateFoundations (foundations, cardsThatCanBeMoved) = cardsThatCanBeMoved ++ [card | card <- foundations, not (elem (sCard card) cardsThatCanBeMoved)]

updateColumns :: (Columns, Deck) -> Columns
updateColumns (columns, cardsThatCanBeMoved) = [[card | card <- column, not (elem card cardsThatCanBeMoved)] | column <- columns]

updateReserve :: (Reserve, Deck) -> Reserve
updateReserve (reserve, cardsThatCanBeMoved) = [reserveCard | reserveCard <- reserve, not (elem reserveCard cardsThatCanBeMoved)]

-- Prevent exception when calling 'head' on an empty column (list) e.g. in doAllPossibleMoves.
removeEmptyColumns :: Columns -> Columns
removeEmptyColumns columns = [column | column <- columns, length column > 0]

-- The purpose of this type is explained in the SBoard Show definition above.
type Hidden = [(Int, Int)]

type Stock = [Card]

columnsInAppendixB :: Columns
columnsInAppendixB = [[(Eight, Diamonds), (Nine, Hearts)], [(Two, Diamonds)],
    [(Ace, Spades), (Two, Spades), (Three, Spades), (Four, Spades), (Five, Spades), (Six, Clubs), (Seven, Clubs), (Eight, Clubs), (Nine, Clubs), (Ten, Diamonds),
    (Jack, Diamonds), (Queen, Diamonds), (King, Diamonds), (Ace, Spades), (Eight, Spades)],
    [(Seven, Clubs), (Eight, Diamonds), (Nine, Diamonds), (Ten, Diamonds), (Jack, Diamonds), (Queen, Diamonds), (King, Diamonds), (Nine, Clubs), (Ten, Hearts), (Jack, Clubs)],
    [(Ace, Hearts), (Two, Hearts), (Three, Hearts), (Four, Hearts), (Five, Hearts), (Six, Diamonds), (Seven, Diamonds), (Queen, Clubs), (King, Hearts)],
    [(Two, Diamonds), (Three, Diamonds), (Four, Diamonds)],
    [(Jack, Clubs), (Queen, Clubs), (King, Clubs), (Two, Spades), (Three, Spades), (Four, Diamonds), (Five, Diamonds), (Six, Diamonds), (Seven, Hearts), (Eight, Clubs), (Nine, Spades),
    (Ten, Clubs), (Ace, Clubs), (Two, Clubs), (Three, Clubs), (Four, Clubs), (Five, Spades)],
    [(Seven, Spades), (Eight, Spades), (Nine, Spades), (Ten, Spades), (Jack, Spades), (Queen, Spades), (King, Spades), (Four, Clubs), (Ten, Clubs), (Ten, Spades)],
    [(Jack, Hearts), (Queen, Hearts)], [(Ace, Clubs), (Two, Clubs)]]

{- Function with the purpose of calculating the remaining stock relative to the cards already located in the columns in appendix B board. Makes sure that
exactly 2 copies of each card are on the playing board since in spider solitaire 2 complete 52 card decks are used.-}
calculateStockForAppendixB :: Stock
calculateStockForAppendixB = [card | card <- shuffle seed pack, iteration <- [0..1], iteration >= countInstances card (columnsInAppendixB ++ foundationsCardsInAppendixB)]
    where
        countInstances card columns = foldr (\column outerCount -> foldr (\item innerCount -> if item == card then innerCount+1 else innerCount) outerCount column) 0 columns
        foundationsCardsInAppendixB = [generateSequenceFromTopCard [] (King, Hearts)]

        seed = 1

-- Helper function for 'calculateStockForAppendixB'. Takes in empty list and top card. Returns deck of all the cards of the same suit under the initial card + initial card.
generateSequenceFromTopCard :: Deck -> Card -> Deck
generateSequenceFromTopCard currentSequence card@(Ace, _) = currentSequence ++ [card]
generateSequenceFromTopCard currentSequence card = generateSequenceFromTopCard (currentSequence ++ [card]) (pCard card)

appendixBBoard :: Board
appendixBBoard = SBoard [(King, Hearts)] columnsInAppendixB [(3, 14), (8, 8)] calculateStockForAppendixB

sDeal :: Int -> Board
sDeal seed = SBoard [] ([[deck !! (50 + i*6 + j) | j <- [0..5]] | i <- [0..3]] ++ [[deck !! (74 + i*5 + j) | j <- [0..4]] | i <- [0..5]]) [(i, 2) | i <- [1..10]] (take 50 deck)
    where
        deck = [shuffledCard | shuffledCard <- shuffle seed [card | iteration <- [1..2], card <- pack]]

findMoves :: Board -> [Board]
findMoves board@(EOBoard foundations columns reserve) = nub (map toFoundations ([board] ++ moveColumnCardsToReserve board ++ moveReserveCardsToColumns board
    ++ moveColumnCardsToDifferentColumns board))

-- Return list of board states after moving all the column heads to reserve, one by one.
moveColumnCardsToReserve :: Board -> [Board]
moveColumnCardsToReserve (EOBoard foundations columns reserve) = if length reserve < 8 then [EOBoard foundations [if i==j then drop 1 column else
    column | (column, j) <- zip columns [0..]] ((head (columns !! i)) : reserve) | i <- [0..(length columns - 1)], length (columns !! i) > 0] else []

moveReserveCardsToColumns :: Board -> [Board]
moveReserveCardsToColumns = undefined

moveColumnCardsToDifferentColumns :: Board -> [Board]
moveColumnCardsToDifferentColumns = undefined