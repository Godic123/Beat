module Beat where

import System.Random

-- kinds of cards
data Card = Missing | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A | Two | S_Joker | B_Joker | Error
    deriving (Ord, Enum, Eq, Read, Show, Bounded)

-- modes of cards that a player can play
-- data Mode = Single | Double | Triple | Bomb | Straight | Triple_Single | Triple_Double | Four_2Single | Four_2Double | Undefined



type Game = [Card] -> [Card] -> [Card] -> Bool -> Bool -> Card -> IO [Char]
type ActionType = Card -> [Card] -> Bool -> Bool -> IO (Card, [Card], Bool, Bool)
type ParseIntoCardType = [Int] -> [Card]
type ParseIntoIntType = [Card] -> [Int]
type ParseIntoInt = [String] -> [Int]


type TournammentState = (Int,Int,Int)   -- wins, losses, ties

--------- Game ---------
play = beat [Three, Three, Four, Five, Six, Six, Eight, Nine, Nine, Nine, Ten, J, J, Q, Q, K, A] [Three, Four, Four, Five, Six, Seven, Seven, Seven, Seven, Eight, Ten, Ten, Ten, J, K, A, Two, Two] [Three, Four, Five, Six, Eight, Eight, Nine, J, Q, Q, K, K, A, A, Two, Two] True True Missing

deck = [Three, Three, Four, Five, Six, Six, Eight, Nine, Nine, Nine, Ten, J, J, Q, Q, K, A, Three, Four, Four, Five, Six, Seven, Seven, Seven, Seven, Eight, Ten, Ten, Ten, J, K, A, Two, Two, Three, Four, Five, Six, Eight, Eight, Nine, J, Q, Q, K, K, A, A, Two, Two]

beat:: Game
beat hand_p1 hand_p2 hand_p3 b1 b2 firstelement= 
    do
    -- for the fist player's turn:
        putStrLn ("Player 1's turn. These are your cards:")
        v <- action firstelement hand_p1 b1 b2
        let (firstelement, hand1_, b1, b2) = v
        -- TBI: we need to check if the hand was modified so that we can tell if this player played
        -- now we have the mode & highest card for the turn
        if hand1_ == []
            then 

                return ("Player 1 has won the game")
            -- if the player1 did not finish their cards on hand, then we proceed to the next player
            else
    -- for the second player's turn:
                do
                    putStrLn ("Player 2's turn. These are your cards:")
                    v <- action firstelement hand_p2 b1 b2
                    let (firstelement,hand2_, b1, b2) = v
                    -- TBI: we need to check if the hand was modified so that we can tell if this player played
                    if (hand2_) == []
                        then return ("Player 2 has won the game")
                    -- if the player1 did not finish their cards on hand, then we proceed to the next player
                        else 
                            do
    -- for the third player's turn:
                                putStrLn ("Player 3's turn. These are your cards:")
                                v <- action firstelement hand_p3 b1 b2
                                let (firstelement, hand3_, b1, b2) = v
                                -- TBI: we need to check if the hand was modified so that we can tell if this player played
                                if (hand3_) == []
                                    then return ("Player 3 has won the game")
                                    else beat hand1_ hand2_ hand3_ b1 b2 firstelement

--------- Action ---------

action :: ActionType
action largestCard hand_b b1 b2=
    do 
        print hand_b
        putStrLn ("What are the cards you want to play? Play 0 to pass. Play the numerical representaion of the card aside from J, Q, K, and A.")
        
        play <- getLine
        --let a = lines play
        --check if the player chose to pass
        if checker [play]
            then
            if  (play == "0")
        -- if the player passes, then everything returns unmodified
                then if (b1 && b2)
                    then return (largestCard, hand_b, False, b2)
                    else if (b1 == False && b2)
                        then return (largestCard, hand_b, b1, False)
                        else 
                            return (largestCard, hand_b, True, True)
        -- otherwise, we get rid of the cards played from the player's hand
                else
        -- checks if the input is in the hand and if it is correct
                    if ((isin (parseIntoIntFromChar [play]) (parseIntoInt hand_b)))
                        then
                        do
        -- hand_a = [Card] after getting rid of cards played
                            let hand_a = (getRidOfPlay (parseIntoIntFromChar [play]) (parseIntoInt hand_b))
        -- playedCard = [Card]
                            let playedCard = parseIntoCard (parseIntoIntFromChar [play])
        -- this firstelement is to be compared with the largestCard so far
                            let firstelement = head playedCard
        -- TBI: determine the mode as well!!
                            if (b1 || b2)
                                then if (firstelement > largestCard)
                                    then return (firstelement, (parseIntoCard hand_a), True, True)
                                    else do 
                                        putStrLn ("Must be bigger than: ")
                                        print largestCard
                                        putStrLn ("Try again.")
                                        action largestCard hand_b b1 b2
                                else 
                                    return (firstelement, (parseIntoCard hand_a), True, True)
                        else 
                        do
                            putStrLn ("Invalid input, try again")
                            action largestCard hand_b b1 b2
            else 
            do
                putStrLn ("Invalid input, try again")
                action largestCard hand_b b1 b2




--------- Helper Functions ---------
                    
isin lst1 lst2 
    | head lst1 `elem` lst2 = True
    | otherwise = False

checker lst 
    | (head lst == "3") = True
    | (head lst == "4") = True
    | (head lst == "5") = True
    | (head lst == "6") = True
    | (head lst == "7") = True
    | (head lst == "8") = True
    | (head lst == "9") = True
    | (head lst == "10") = True
    | (head lst == "J") = True
    | (head lst == "Q") = True
    | (head lst == "K") = True
    | (head lst == "A") = True
    | (head lst == "2") = True
    | (head lst == "0") = True
    | otherwise = False

-- gets rid of the played cards from the hand--
getRidOfPlay lst hand = deleteElems lst hand

deleteElems [] lst = lst
deleteElems  _ [] = []
deleteElems xs zs
    | head xs == head zs = deleteElems (tail xs) (tail zs)
    | otherwise = head zs : deleteElems xs (tail zs)

deleteElem :: Int -> [a] -> [a]
deleteElem _ [] = []
deleteElem x zs | x > 0 = take (x-1) zs ++ drop x zs
                | otherwise = zs
--

-- parse the given input into Card and return the Card--
parseIntoCard :: ParseIntoCardType
parseIntoCard [] = []
parseIntoCard (h:t)
    | (h == 3) = Three:(parseIntoCard t)
    | (h == 4) = Four:(parseIntoCard t)
    | (h == 5) = Five:(parseIntoCard t)
    | (h == 6) = Six:(parseIntoCard t)
    | (h == 7) = Seven:(parseIntoCard t)
    | (h == 8) = Eight:(parseIntoCard t)
    | (h == 9) = Nine:(parseIntoCard t)
    | (h == 10) = Ten:(parseIntoCard t)
    | (h == 11) = J:(parseIntoCard t)
    | (h == 12) = Q:(parseIntoCard t)
    | (h == 13) = K:(parseIntoCard t)
    | (h == 1) = A:(parseIntoCard t)
    | (h == 2) = Two:(parseIntoCard t)
    | (h == 20) = S_Joker:(parseIntoCard t)
    | (h == 21) = B_Joker:(parseIntoCard t)
    | otherwise = []
--
-- parse the given input into number and return the list of number--
parseIntoInt :: ParseIntoIntType
parseIntoInt [] = []
parseIntoInt (h:t)
    | (h == Three) = 3:(parseIntoInt t)
    | (h == Four) = 4:(parseIntoInt t)
    | (h == Five) = 5:(parseIntoInt t)
    | (h == Six) = 6:(parseIntoInt t)
    | (h == Seven) = 7:(parseIntoInt t)
    | (h == Eight) = 8:(parseIntoInt t)
    | (h == Nine) = 9:(parseIntoInt t)
    | (h == Ten) = 10:(parseIntoInt t)
    | (h == J) = 11:(parseIntoInt t)
    | (h == Q) = 12:(parseIntoInt t)
    | (h == K) = 13:(parseIntoInt t)
    | (h == A) = 1:(parseIntoInt t)
    | (h == Two) = 2:(parseIntoInt t)
    | (h == S_Joker) = 20:(parseIntoInt t)
    | (h == B_Joker) = 21:(parseIntoInt t)
    | otherwise = []
--

parseIntoIntFromString :: ParseIntoInt
parseIntoIntFromString [] = []
parseIntoIntFromString (h:t)
    | (h == "Three") = 3:(parseIntoIntFromString t)
    | (h == "Four") = 4:(parseIntoIntFromString t)
    | (h == "Five") = 5:(parseIntoIntFromString t)
    | (h == "Six") = 6:(parseIntoIntFromString t)
    | (h == "Seven") = 7:(parseIntoIntFromString t)
    | (h == "Eight") = 8:(parseIntoIntFromString t)
    | (h == "Nine") = 9:(parseIntoIntFromString t)
    | (h == "Ten") = 10:(parseIntoIntFromString t)
    | (h == "J") = 11:(parseIntoIntFromString t)
    | (h == "Q") = 12:(parseIntoIntFromString t)
    | (h == "K") = 13:(parseIntoIntFromString t)
    | (h == "A") = 1:(parseIntoIntFromString t)
    | (h == "Two") = 2:(parseIntoIntFromString t)
    | (h == "S_Joker") = 20:(parseIntoIntFromString t)
    | (h == "B_Joker") = 21:(parseIntoIntFromString t)
    | otherwise = []
--
-------------------------

parseString [] = []
parseString (h:t)
    | h /= "," = parseString(t)
    | otherwise = (read(h) :: Int):parseString(t)

parseIntoIntFromChar:: ParseIntoInt
parseIntoIntFromChar [] = []
parseIntoIntFromChar (h:t)
    | (h == "3") = [3]
    | (h == "4") = [4]
    | (h == "5") = [5]
    | (h == "6") = [6]
    | (h == "7") = [7]
    | (h == "8") = [8]
    | (h == "9") = [9]
    | (h == "10") = [10]
    | (h == "J") = [11]
    | (h == "Q") = [12]
    | (h == "K") = [13]
    | (h == "A") = [1]
    | (h == "2") = [2]
    | otherwise = []
--

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

start = 
    do 
        v <- shuffle deck
        return (splitEvery 18 v)

playgame = 
    do
        putStrLn ("Shuffling...")
        v <- start
        beat (quicksort (head v)) (quicksort (head(tail v))) (quicksort (head(tail(tail v)))) True True Missing

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs