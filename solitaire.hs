{- Author: Jedrzej Golebiewski
COM2108 Functional Programming Grading Assignment -}

import System.Random
import Data.List
import Data.Maybe
import Debug.Trace

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
data Board = EOBoard Foundations Columns Reserve | SBoard Foundations Columns Hidden Stock

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

instance Eq Board where
    EOBoard foundations1 columns1 reserve1 == EOBoard foundations2 columns2 reserve2 =
        null (foundations1 \\ foundations2) && null (foundations2 \\ foundations1) &&
        null (columns1 \\ columns2) && null (columns2 \\ columns1) &&
        null (reserve1 \\ reserve2) && null (reserve2 \\ reserve1)

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

{- The 'toFoundations' function recursively calls itself until no cards can be moved. During each call, it creates a list with the cards that can be moved,
updates 'foundations' with these cards, updates 'columns' and 'reserve' by removing the cards that can be moved. If no cards can be moved, the function terminates.-}
toFoundations :: Board -> Board
toFoundations board@(EOBoard foundations columns reserve) = boardToReturn
    where
        cardsThatCanBeMoved = [head column | column <- removeEmptyColumns columns, isAce (head column) || elem (pCard (head column)) foundations] ++
            [reserveCard | reserveCard <- reserve, isAce reserveCard || elem (pCard reserveCard) foundations]

        updatedFoundations = updateFoundations (foundations, cardsThatCanBeMoved)
        updatedColumns = updateColumns (columns, cardsThatCanBeMoved)
        updatedReserve = updateReserve (reserve, cardsThatCanBeMoved)

        boardToReturn = if null cardsThatCanBeMoved then board else toFoundations (EOBoard updatedFoundations updatedColumns updatedReserve)

-- Helper functions for 'toFoundations'. Functionality explained above.
updateFoundations :: (Foundations, Deck) -> Foundations
updateFoundations (foundations, cardsThatCanBeMoved) = cardsThatCanBeMoved ++ [card | card <- foundations, not (elem (sCard card) cardsThatCanBeMoved)]

updateColumns :: (Columns, Deck) -> Columns
updateColumns (columns, cardsThatCanBeMoved) = [[card | card <- column, not (elem card cardsThatCanBeMoved)] | column <- columns]

updateReserve :: (Reserve, Deck) -> Reserve
updateReserve (reserve, cardsThatCanBeMoved) = [reserveCard | reserveCard <- reserve, not (elem reserveCard cardsThatCanBeMoved)]

-- Prevent exception when calling 'head' on an empty column (list) e.g. in toFoundations.
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

-- Finds all possible moves from a board state, removes duplicates and potentially removes original state from return list
findMoves :: Board -> [Board]
findMoves board = [newBoard | newBoard <- allPossibleMoves, newBoard /= board]
    where
        allPossibleMoves = nub (map toFoundations ([board] ++ moveColumnCardsToReserve board ++ moveReserveCardsToColumns board
            ++ moveColumnCardsToDifferentColumns board))

-- Return list of board states after moving all the column heads to reserve, one by one.
moveColumnCardsToReserve :: Board -> [Board]
moveColumnCardsToReserve (EOBoard foundations columns reserve)
    | length reserve >= 8 = []
    | otherwise = [EOBoard foundations [if innerColumn == outerColumn then drop 1 (columns !! innerColumn) else (columns !! innerColumn) | innerColumn <-
    [0..(length columns - 1)]] ((head (columns !! outerColumn)) : reserve) | outerColumn <- [0..(length columns - 1)], length (columns !! outerColumn) > 0]

moveReserveCardsToColumns :: Board -> [Board]
moveReserveCardsToColumns (EOBoard foundations columns reserve) = [EOBoard foundations [if innerColumn == outerColumn then (reserveCard:columns !! innerColumn)
    else columns !! innerColumn | innerColumn <- [0..(length columns - 1)]] (filter (/= reserveCard) reserve) | reserveCard <- reserveWithoutKings,
    outerColumn <- [0..(length columns - 1)], length (columns !! outerColumn) > 0 && pCard (head (columns !! outerColumn)) == reserveCard] ++
    [EOBoard foundations [if innerColumn == outerColumn then [reserveCard] else columns !! innerColumn | innerColumn <- [0..(length columns - 1)]]
    (filter (/= reserveCard) reserve) | reserveCard <- reserveOnlyKings, outerColumn <- [0..(length columns - 1)], length (columns !! outerColumn) == 0]
        where
            reserveWithoutKings = filter (not.isKing) reserve
            reserveOnlyKings = filter isKing reserve

moveColumnCardsToDifferentColumns :: Board -> [Board]
moveColumnCardsToDifferentColumns (EOBoard foundations columns reserve) = concat [concat [[EOBoard foundations [if col == outerColumn then drop subsetIndexOfCol (columns !! outerColumn)
    else if col == innerColumn then take subsetIndexOfCol (columns !! outerColumn) ++ (columns !! innerColumn) else (columns !! col)| col <- [0..(length columns - 1)]] reserve |
    innerColumn <- [0..(length columns - 1)], innerColumn /= outerColumn, length (columns !! innerColumn) == 0 && isKing (columns !! outerColumn !! (subsetIndexOfCol - 1)) ||
    length (columns !! innerColumn) > 0 && sCard (columns !! outerColumn !! (subsetIndexOfCol - 1)) == head (columns !! innerColumn)] | subsetIndexOfCol <-
    [1..(length (columns !! outerColumn))], isInfixOf (take subsetIndexOfCol (columns !! outerColumn)) (reverse (generateSequenceFromTopCard [] (columns !! outerColumn !!
    (subsetIndexOfCol - 1))))] | outerColumn <- [0..(length columns - 1)]]

{- It would be best to change the definition of this function to allow the input of a list of boards i.e. the history of past board states,
so that these can be checked against to avoid infinite loops, but as per the assignment instructions, I kept the definition as it is right now
and created helper functions to overcome the issue. -}
chooseMove :: Board -> Maybe Board
chooseMove board
    | findMoves board == [] = Nothing
    | nextMoveTemp == ((1000000, -1), EOBoard [] [[]] []) = Nothing
    | otherwise = Just nextMove
        where
            -- Use lazy evaluation to avoid combinatorial explosion
            nextMoveTemp = foldr (\((prevElemDepth, prevBestSolution), prevBoard) ((depth, bestSolution), currentBoard) -> if prevBestSolution == bestSolution then
                (if prevElemDepth < depth then ((prevElemDepth, prevBestSolution), prevBoard) else ((depth, bestSolution), currentBoard)) else (if prevBestSolution > bestSolution then ((prevElemDepth, prevBestSolution), prevBoard) else ((depth, bestSolution), currentBoard))) ((1000000, -1), EOBoard [] [[]] [])
                [(fromJust (calculateDepthOfOptimalSolution move [board] 1), move) | move <- findMoves board, (not.isNothing) (calculateDepthOfOptimalSolution move [board] 1)]
            
            ((_, foundations), nextMove) = nextMoveTemp

calculateDepthOfOptimalSolution :: Board -> [Board] -> Int -> Maybe (Int, Int)
calculateDepthOfOptimalSolution board boardHistory depth
    | findMoves board == [] = Just (depth, cardsInFoundations board)
    | elem board boardHistory = Nothing
    | depth >= 3 = Just (depth, cardsInFoundations board)
    | bestSolutionIncurrentBranch == (1000000, -1) = Nothing
    | otherwise = Just bestSolutionIncurrentBranch
        where
            -- Use lazy evaluation to avoid combinatorial explosion
            bestSolutionIncurrentBranch = foldr (\(prevElemDepth, prevBestSolution) (depth, bestSolution) -> if prevBestSolution == bestSolution then
                ((min prevElemDepth depth), bestSolution) else (if prevBestSolution > bestSolution then (prevElemDepth, prevBestSolution) else (depth, bestSolution))) (1000000, -1)
                (map (fromJust) (filter (not.isNothing)
                [calculateDepthOfOptimalSolution move (board:boardHistory) (depth+1) | move <- findMoves board]))

cardsInFoundations :: Board -> Int
cardsInFoundations (EOBoard foundations columns reserve) = sum [length (generateSequenceFromTopCard [] topCard) | topCard <- foundations]

bestMovesFromBoardInOrder :: Board -> [Board]
bestMovesFromBoardInOrder board = reverse $ findMoves board

haveWon :: Board -> Bool
haveWon (EOBoard foundations columns reserve) = length reserve == 0 && 
    foldr (\col totalCardsAmount -> length col + totalCardsAmount) 0 columns == 0

playSolitaire :: Board -> Int -> Int
playSolitaire board n
    | isNothing nextBoard = cardsInFoundations board
    | otherwise = if n `mod` 35 == 0 then traceShow board (playSolitaire (fromJust nextBoard) (n+1)) else playSolitaire (fromJust nextBoard) (n+1)
        where
            nextBoard = chooseMove board

-- Float precision will suffice
{-analyseEO :: Int -> Int -> (Int, Float)
analyseEO seed noOfGames = (length (filter (==52) results), (fromIntegral (sum results)) / (fromIntegral (length results)))
    where
        results = [playSolitaire (eODeal shuffleSeed) | (shuffleSeed, _) <- zip (randoms (mkStdGen seed) :: [Int]) [1..noOfGames]]-}

{-TODO
pick good search strategy in chooseMove
-}