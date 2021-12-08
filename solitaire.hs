{- Author: Jedrzej Golebiewski
COM2108 Functional Programming Grading Assignment -}

import System.Random
import Data.List
import Data.Maybe

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
        [(Five, Diamonds), (Queen, Spades), (Three, Diamonds), (Five, Spades), (Six, Spades), (Seven, Hearts)],
        [(King, Hearts), (Ten, Diamonds), (Seven, Spades), (Queen, Diamonds), (Five, Hearts), (Eight, Diamonds)],
        [(Jack, Spades), (Six, Hearts), (Seven, Clubs), (Eight, Spades), (Ten, Clubs), (Queen, Clubs)],
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
    [(Jack, Clubs), (Queen, Clubs), (King, Clubs), (Two, Spades), (Three, Spades), (Four, Diamonds), (Five, Diamonds), (Six, Diamonds), (Seven, Hearts), (Eight, Clubs),
    (Nine, Spades), (Ten, Clubs), (Ace, Clubs), (Two, Clubs), (Three, Clubs), (Four, Clubs), (Five, Spades)],
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

-- Helper function for 'calculateStockForAppendixB' as well as other functions. Takes in empty list and top card. Returns deck of all the cards of the same suit
-- under the initial card + initial card.
generateSequenceFromTopCard :: Deck -> Card -> Deck
generateSequenceFromTopCard currentSequence card@(Ace, _) = currentSequence ++ [card]
generateSequenceFromTopCard currentSequence card = generateSequenceFromTopCard (currentSequence ++ [card]) (pCard card)

appendixBBoard :: Board
appendixBBoard = SBoard [(King, Hearts)] columnsInAppendixB [(3, 14), (8, 8)] calculateStockForAppendixB

sDeal :: Int -> Board
sDeal seed = SBoard [] ([[deck !! (50 + i*6 + j) | j <- [0..5]] | i <- [0..3]] ++ [[deck !! (74 + i*5 + j) | j <- [0..4]] | i <- [0..5]]) [(i, 2) | i <- [1..10]] (take 50 deck)
    where
        deck = [shuffledCard | shuffledCard <- shuffle seed [card | iteration <- [1..2], card <- pack]]

-- Finds all possible moves from a board state, removes duplicates and potentially removes original state from return list if toFoundations didn't change the original state
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

-- Return list of board states after moving all the possible reserve cards to columns, one by one.
moveReserveCardsToColumns :: Board -> [Board]
moveReserveCardsToColumns (EOBoard foundations columns reserve) = [EOBoard foundations [if innerColumn == outerColumn then (reserveCard:columns !! innerColumn)
    else columns !! innerColumn | innerColumn <- [0..(length columns - 1)]] (filter (/= reserveCard) reserve) | reserveCard <- reserveWithoutKings,
    outerColumn <- [0..(length columns - 1)], length (columns !! outerColumn) > 0 && pCard (head (columns !! outerColumn)) == reserveCard] ++
    [EOBoard foundations [if innerColumn == outerColumn then [reserveCard] else columns !! innerColumn | innerColumn <- [0..(length columns - 1)]]
    (filter (/= reserveCard) reserve) | reserveCard <- reserveOnlyKings, outerColumn <- [0..(length columns - 1)], length (columns !! outerColumn) == 0]
        where
            reserveWithoutKings = filter (not.isKing) reserve
            reserveOnlyKings = filter isKing reserve

-- Return list of board states after moving column cards to other columns. Multiple cards stacked on top of each other can be moved at once if eight-off rules allow for it.
moveColumnCardsToDifferentColumns :: Board -> [Board]
moveColumnCardsToDifferentColumns (EOBoard foundations columns reserve) = concat [concat [[EOBoard foundations [if col == outerColumn then drop subsetIndexOfCol
    (columns !! outerColumn) else if col == innerColumn then take subsetIndexOfCol (columns !! outerColumn) ++ (columns !! innerColumn) else (columns !! col) |
    col <- [0..(length columns - 1)]] reserve | innerColumn <- [0..(length columns - 1)], innerColumn /= outerColumn, length (columns !! innerColumn) == 0 &&
    isKing (columns !! outerColumn !! (subsetIndexOfCol - 1)) || length (columns !! innerColumn) > 0 && sCard (columns !! outerColumn !! (subsetIndexOfCol - 1))
    == head (columns !! innerColumn)] | subsetIndexOfCol <- [1..(length (columns !! outerColumn))], isInfixOf (take subsetIndexOfCol (columns !! outerColumn))
    (reverse (generateSequenceFromTopCard [] (columns !! outerColumn !! (subsetIndexOfCol - 1))))] | outerColumn <- [0..(length columns - 1)]]

{- chooseMove creates a board state tree with the board passed in as argument as the root of the tree. Starting with a max search depth of 1, the function iteratively
increases max search depth by one each time until it finds a state in which the number of cards in foundations is greater than in the board at the root of the tree.
This solution saves computational resources when a deep search is not necessary to find a board state with a higher number of cards in foundations.
If the function can't find a move within the max search depth limit (defined as a constant), it returns Nothing. More information can be found in the report. -}
chooseMove :: Board -> Maybe Board
chooseMove board
    | findMoves board == [] = Nothing
    | nextMoveFiltered == [] = Nothing
    | otherwise = Just nextMove
        where
            nextMoveTemp = [foldr (\((depth, bestSolution), currentBoard) ((prevElemDepth, prevBestSolution), prevBoard) -> if prevBestSolution == bestSolution then
                (if prevElemDepth < depth then ((prevElemDepth, prevBestSolution), prevBoard) else ((depth, bestSolution), currentBoard)) else if
                prevBestSolution > bestSolution then ((prevElemDepth, prevBestSolution), prevBoard) else ((depth, bestSolution), currentBoard)) ((1000000, -1),
                EOBoard [] [[]] []) [(fromJust (calculateDepthOfOptimalSolution move [board] 1 maxDepthOfIteration), move) | move <- findMoves board, (not.isNothing)
                (calculateDepthOfOptimalSolution move [board] 1 maxDepthOfIteration)] | maxDepthOfIteration <- [1..maxDepthOfSearch]]
            
            nextMoveFiltered = filter (\((_, cardsInBestFoundations), _) -> cardsInBestFoundations > countCardsInFoundations board) nextMoveTemp

            ((_, _), nextMove) = head nextMoveFiltered

{- Helper function for chooseMove. Maintains history of past board states to prune any already visited board states which will lead to infinite loops. Although this is not
necessary due to the chooseMove function returning Nothing if an acceptable state cannot be found, in specific cases it cuts off branches that don't need to searched thereby
saving computational resources. More information can be found in the report. -}
calculateDepthOfOptimalSolution :: Board -> [Board] -> Int -> Int -> Maybe (Int, Int)
calculateDepthOfOptimalSolution board boardHistory depth maxDepth
    | elem board boardHistory = Nothing
    | depth >= maxDepth || bestSolution == (-1) || bestSolution == countCardsInFoundations board = Just (depth, countCardsInFoundations board)
    | otherwise = Just (bestDepth, bestSolution)
        where
            (bestDepth, bestSolution) = foldr (\(depth, bestSolution) (prevElemDepth, prevBestSolution) -> if prevBestSolution == bestSolution then
                ((min prevElemDepth depth), bestSolution) else (if prevBestSolution > bestSolution then (prevElemDepth, prevBestSolution) else (depth, bestSolution))) (1000000, -1)
                (map (fromJust) (filter (not.isNothing)
                [calculateDepthOfOptimalSolution move (board:boardHistory) (depth+1) maxDepth | move <- findMoves board]))

countCardsInFoundations :: Board -> Int
countCardsInFoundations (EOBoard foundations columns reserve) = sum [length (generateSequenceFromTopCard [] topCard) | topCard <- foundations]

haveWon :: Board -> Bool
haveWon board = countCardsInFoundations board == 52

playSolitaire :: Board -> Int
playSolitaire board
    | isNothing nextBoard = countCardsInFoundations board
    | otherwise = playSolitaire (fromJust nextBoard)
        where
            nextBoard = chooseMove board

-- Float precision will suffice
-- In the return tuple, the first Int is the number of games won and the second Int is the average number of cards in foundations at the end of each game.
analyseEO :: Int -> Int -> (Int, Float)
analyseEO seed noOfGames = (length (filter (==52) results), (fromIntegral (sum results)) / (fromIntegral (length results)))
    where
        results = [playSolitaire (eODeal shuffleSeed) | (shuffleSeed, _) <- zip (randoms (mkStdGen seed) :: [Int]) [1..noOfGames]]

-- Max depth of search tree. Higher value = better score and more games won but more time required. Max depth 5 has a good balance between score and execution time.
maxDepthOfSearch = 5

{- Paste the contents of this file, including this comment, into your source file, below all
    of your code. You can change the indentation to align with your own, but other than this,
    ONLY make changes as instructed in the comments.
-}
-- Constants that YOU must set:
studentName = "Jedrzej Golebiewski"
studentNumber = "200178868"
studentUsername = "acf20jg"

initialBoardDefined = appendixABoard {- replace XXX with the name of the constant that you defined
                            in step 3 of part 1 -}
secondBoardDefined = appendixBBoard {- replace YYY with the constant defined in step 5 of part 1,
                            or if you have chosen to demonstrate play in a different game
                            of solitaire for part 2, a suitable contstant that will show
                            your play to good effect for that game -}

{- Beyond this point, the ONLY change you should make is to change the comments so that the
    work you have completed is tested. DO NOT change anything other than comments (and indentation
    if needed). The comments in the template file are set up so that only the constant eight-off
    board from part 1 and the toFoundations function from part 1 are tested. You will probably
    want more than this tested.

    CHECK with Emma or one of the demonstrators if you are unsure how to change this.

    If you mess this up, your code will not compile, which will lead to being awarded 0 marks
    for functionality and style.
-}

main :: IO()
main =
    do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

        {- Move the start comment marker below to the appropriate position.
        If you have completed ALL the tasks for the assignment, you can
        remove the comments from the main function entirely.
        DO NOT try to submit/run non-functional code - you will receive 0 marks
        for ALL your code if you do, even if *some* of your code is correct.
        -}

        let boards = findMoves board      -- show that findMoves is working
        putStrLn "***The possible next moves after that:"
        print boards

        let chosen = chooseMove board     -- show that chooseMove is working
        putStrLn "***The chosen move from that set:"
        print chosen

        putStrLn "***Now showing a full game"     -- display a full game
        score <- displayGame initialBoardDefined 0
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

        {- start comment marker - move this if appropriate

        putStrLn "***Now showing a full game for alternative solitaire"
        score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

        -}

{- displayGame takes a Board and move number (should initially be 0) and
    displays the game step-by-step (board-by-board). The result *should* be
    the same as performing playSolitaire on the initial board, if it has been
    implemented correctly.
    DO NOT CHANGE THIS CODE other than aligning indentation with your own.
-}
displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
        then return "A WIN"
        else
            do
                putStr ("Move " ++ show n ++ ": " ++ show board)
                let maybeBoard = chooseMove board
                if isJust maybeBoard then
                    do
                        let (Just newBoard) = maybeBoard
                        displayGame newBoard (n+1)
                else
                    do
                        let score = show (playSolitaire board)
                        return score
