{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Arrow
import Data.Char ( isDigit )
import Data.Maybe ( fromJust )
import Data.Ord ( comparing )
import Data.List
import Util

lookup' :: Eq a => a -> [(a,b)] -> b
lookup' = (fromJust .) . lookup

data Suit = Club | Diamond | Heart | Spade
          deriving (Eq, Ord)
instance Show Suit where
    show s = return $ lookup' s $ zip suits suitsChar
instance Read Suit where
    readsPrec _ (c:cs) = maybe [] (\s -> [(s,cs)]) $ lookup c $ zip suitsChar suits

data Rank = N Int | Jack | Queen | King | Ace
          deriving (Eq, Ord)
instance Show Rank where
    show r = return $ lookup' r $ zip ranks ranksChar
instance Read Rank where
    readsPrec _ (c:cs) = maybe [] (\r -> [(r, cs)]) $ lookup c $ zip ranksChar ranks
instance Bounded Rank where
    minBound = N 2
    maxBound = Ace
instance Enum Rank where
    toEnum   i = lookup' i $ zip [2..] ranks
    fromEnum e = lookup' e $ zip ranks [2..]

data Card = C { rank :: Rank, suit :: Suit } deriving (Eq, Ord)
instance Show Card where
    show (C r s) = show r ++ show s
instance Read Card where
    readsPrec _ ccs@(c1:c2:cs)
        | not (null r' || null s') = let r = fst $ head r'
                                         s = fst $ head s'
                                     in [(C r s, cs)]
        | otherwise = []
        where
          r' = readsPrec undefined [c1] :: [(Rank, String)]
          s' = readsPrec undefined [c2] :: [(Suit, String)]

suits :: [Suit]
suits = [ Club, Diamond, Heart, Spade ]
suitsChar :: String
suitsChar = "CDHS"

ranks :: [Rank]
ranks = map N [2..10] ++ [Jack, Queen, King, Ace]
ranksChar :: String
ranksChar = concatMap show [2..9] ++ "TJQKA"

deck :: [Card]
deck = [ C r s | s <- suits, r <- ranks ]

data Hand = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight |
            Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Eq, Ord, Show)
{-
data Hand = HighCard [Card] | OnePair [Card] [Card] -- hand cards and the rest
-}

whatHand :: [Card] -> Hand
whatHand cs
    | isOnePair      cs = OnePair
    | isTwoPairs     cs = TwoPairs
    | isThreeOfAKind cs = ThreeOfAKind
    | isStraight     cs = Straight
    | isFlush        cs = Flush
    | isFullHouse    cs = FullHouse
    | isFourOfAKind  cs = FourOfAKind
    | isStraight     cs = StraightFlush
    | isRoyalFlush   cs = RoyalFlush
    | otherwise         = HighCard

isOnePair, isTwoPairs, isThreeOfAKind, isStraight, isFlush, isFullHouse,
  isFourOfAKind, isStraightFlush, isRoyalFlush :: [Card] -> Bool
isOnePair = (1 ==) . length . filter ((2 ==) . length) . group . sort
isTwoPairs = (2 ==) . length . filter ((2 ==) . length) . group . sort
isThreeOfAKind = (1 ==) . length . filter ((3 ==) . length) . group . sort
isStraight = isConsecutive . map rank
isFlush = allSame . map suit
isFullHouse = (&&) <$> isOnePair <*> isThreeOfAKind
isFourOfAKind = (1 ==) . length . filter ((4 ==) . length) . group . sort
isStraightFlush = (&&) <$> isStraight <*> isFlush
isRoyalFlush = (&&) <$> isStraightFlush <*> (Ace `elem`) . map rank

isConsecutive :: (Ord a, Enum a) => [a] -> Bool
isConsecutive = sort >>> map fromEnum >>> tail &&& id >>> uncurry zip >>> all ((1==) . uncurry (-))

instance Ord [Card] where
    compare = comparing whatHand 

--winner :: [Card] -> [Card] -> Ordering
--winner xs ys = campare

deals :: IO [([Card], [Card])]
deals = map (splitAt 5 . map read . words) . lines <$> readFile "poker.txt"

search p = (filter p .) . (++) <$> fmap (map fst) deals <*> fmap (map snd) deals

