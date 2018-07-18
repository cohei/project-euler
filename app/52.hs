import Control.Applicative
import Data.List

import Utility (allSame, toDigits)

multiplier :: Int -> [Int]
multiplier n = (*) <$> [1..6] <*> [n]

main = print . head . filter (allSame . map (sort . toDigits) . multiplier) $ [1..]
-- 142857
