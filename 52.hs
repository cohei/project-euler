import Control.Applicative
import Data.List
import Util

multiplier :: Int -> [Int]
multiplier n = (*) <$> [1..6] <*> [n]

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (x==) xs

main = print . head . filter (allSame . map (sort . toDigits) . multiplier) $ [1..]
-- 142857

