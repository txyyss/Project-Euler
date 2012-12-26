-- In the card game poker, a hand consists of five cards and are
-- ranked, from lowest to highest, in the following way:

-- High Card: Highest value card.
-- One Pair: Two cards of the same value.
-- Two Pairs: Two different pairs.
-- Three of a Kind: Three cards of the same value.
-- Straight: All cards are consecutive values.
-- Flush: All cards of the same suit.
-- Full House: Three of a kind and a pair.
-- Four of a Kind: Four cards of the same value.
-- Straight Flush: All cards are consecutive values of same suit.
-- Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
-- The cards are valued in the order:
-- 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

-- If two players have the same ranked hands then the rank made up of
-- the highest value wins; for example, a pair of eights beats a pair
-- of fives (see example 1 below). But if two ranks tie, for example,
-- both players have a pair of queens, then highest cards in each hand
-- are compared (see example 4 below); if the highest cards tie then
-- the next highest cards are compared, and so on.

-- Consider the following five hands dealt to two players:

-- Hand	 	Player 1	 	Player 2	 	Winner
-- 1	 	5H 5C 6S 7S KD          2C 3S 8S 8D TD          Player 2
--              Pair of Fives           Pair of Eights
--  	
-- 2	 	5D 8C 9S JS AC          2C 5C 7D 8S QH          Player 1
--              Highest card Ace        Highest card Queen
--  	
-- 3	 	2D 9C AS AH AC          3D 6D 7D TD QD          Player 2
--              Three Aces              Flush with Diamonds
--  	
-- 4	 	4D 6S 9H QH QC          3D 6D 7H QD QS          Player 1
--              Pair of Queens          Pair of Queens
--              Highest card Nine       Highest card Seven 

-- 5	 	2H 2D 4C 4D 4S          3C 3D 3S 9S 9D          Player 1
--              Full House              Full House
--              With Three Fours        with Three Threes

-- The file, poker.txt, contains one-thousand random hands dealt to
-- two players. Each line of the file contains ten cards (separated by
-- a single space): the first five are Player 1's cards and the last
-- five are Player 2's cards. You can assume that all hands are valid
-- (no invalid characters or repeated cards), each player's hand is in
-- no specific order, and in each hand there is a clear winner.

-- How many hands does Player 1 win?

{-# LANGUAGE NoMonomorphismRestriction #-}
module Euler054 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec
import Data.List

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Enum, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq,Ord,Enum,Show)

data Card = Card { rank :: Rank, suit :: Suit } deriving Eq

instance Show Card where show (Card rank suit) = "Card " ++ show rank ++ " " ++ show suit

isFlush :: [Card] -> Bool
isFlush h = and [a == b |  a <- allSuits, b <- allSuits]
  where allSuits = map suit h

isStraight :: [Card] -> Bool
isStraight h = all (\(a, b) -> b == succ a) (zip (init s) (tail s))
  where s = sort $ map rank h
        
isRoyalFlush :: [Card] -> Bool
isRoyalFlush h = ([Ten,Jack,Queen,King,Ace] == sort (map rank h)) && isFlush h

isStraightFlush :: [Card] -> Bool
isStraightFlush h = isFlush h && isStraight h

sameKindNum :: [Card] -> [(Rank, Int)]
sameKindNum h = zip r $ map (\x -> length $ filter x r) funcs
  where r = map rank h
        funcs = map (\x -> (==x)) r

isSameKind :: Int -> [Card] -> Bool
isSameKind i = any (\(a,b) -> b==i) . sameKindNum

isFullHouse :: [Card] -> Bool
isFullHouse h = helper 2 && helper 3
  where kindNum = sameKindNum h
        helper i = any (\(a,b)->b==i) kindNum

isTwoPairs :: [Card] -> Bool
isTwoPairs = (==4) . length . filter (\(a,b) -> b == 2) . sameKindNum

compareRank :: Ord a => [a] -> [a] -> Ordering
compareRank r1 r2 = helper compareResult
  where sr1 = sort r1
        sr2 = sort r2
        compareResult = filter (/= EQ) $ zipWith compare sr1 sr2
        helper [] = EQ
        helper x = last x

compareOrderedRank :: Ord a => [a] -> [a] -> Ordering
compareOrderedRank r1 r2 = helper compareResult
  where compareResult = filter (/= EQ) $ zipWith compare r1 r2
        helper [] = EQ
        helper x = head x

filterRank :: [Int] -> [(Rank, Int)] -> [Rank]
filterRank l rs = map (\x -> fst $ head $ filter (\(a,b)->b==x) rs) l

filterRestRank :: Int -> [(Rank, Int)] -> [Rank]
filterRestRank i rs = prefered ++ rest
  where (rs1, rs2) = partition (\(a,b)->b==i) rs
        getDistinctRanks = reverse . sort . map fst . Set.toList . Set.fromList
        prefered = getDistinctRanks rs1
        rest = getDistinctRanks rs2
        
judgePattern :: [Card] -> [Bool]
judgePattern = swing map [isRoyalFlush, isStraightFlush, isSameKind 4, isFullHouse, isFlush,
                          isStraight, isSameKind 3, isTwoPairs, isSameKind 2]
  where swing = flip . (. flip id)

compareCard :: ([Card], [Card]) -> Ordering
compareCard (c1, c2) = helper temp
  where p1 = judgePattern c1
        p2 = judgePattern c2
        temp = filter (\(a,b,c)->b || c) $ zip3 [1..] p1 p2
        r1 = map rank c1
        r2 = map rank c2
        rankResult = compareRank r1 r2
        k1 = sameKindNum c1
        k2 = sameKindNum c2
        helper [] = rankResult
        helper a = judge $ head a
        judgeRank l = compareOrderedRank or1 or2
          where or1 = filterRank l k1
                or2 = filterRank l k2
        judgeRestRank i = compareOrderedRank or1 or2
          where or1 = filterRestRank i k1
                or2 = filterRestRank i k2
        judge (_, True, False) = GT
        judge (_, False, True) = LT
        judge (1, _, _) = EQ
        judge (2, _, _) = rankResult
        judge (3, _, _) = judgeRank [4,1]
        judge (4, _, _) = judgeRank [3,2]
        judge (5, _, _) = rankResult
        judge (6, _, _) = rankResult
        judge (7, _, _) = judgeRestRank 3
        judge (8, _, _) = judgeRestRank 2
        judge (9, _, _) = judgeRestRank 2
          
---------------------- Parser Code ----------------------

convertToRank :: Char -> Rank
convertToRank = (Map.!) rMap
  where rMap = Map.fromList r
        r = [('2', Two), ('3', Three), ('4', Four), ('5', Five), ('6', Six), ('7', Seven),
             ('8', Eight), ('9', Nine), ('T', Ten), ('J', Jack), ('Q', Queen), ('K', King), ('A', Ace)]

convertToSuit :: Char -> Suit
convertToSuit = (Map.!) sMap
  where sMap = Map.fromList s
        s = [('C', Clubs), ('D', Diamonds), ('H', Hearts), ('S', Spades)]

cardParser = do
  r <- oneOf "23456789TJQKA"
  s <- oneOf "CDHS"
  return $ Card (convertToRank r) $ convertToSuit s

handsParser = endBy (sepBy cardParser (char ' ')) newline

dealWithHand :: Either ParseError [[Card]] -> Int
dealWithHand (Left a) = 0
dealWithHand (Right b) = length $ filter (==LT) $ map (compareCard . splitAt 5) b

result054 = do
  content <- readFile "data/poker.txt"
  return $ dealWithHand $ parse handsParser "" content
