import Data.List (maximumBy)
import Data.Map hiding (lookup, map, (!))
import Data.Array

nMax :: Num a => a
nMax = 1000000

collatz :: Integer -> Integer
collatz n = if even n then div n 2 else 3*n+1

collatzCount :: Integer -> Int
collatzCount 1 = 0
collatzCount n = 1 + collatzCount (collatz n)

main = print $ findMax $ fromList [(collatzCount x, x) | x <- [1..1000000]]

{-
collatzStep :: Integer -> (Integer,Integer)
collatzStep n | n' <= nMax = (n', 1)
              | otherwise   = let (n'',k) = collatzStep n' in (n'',k+1)
    where n' = collatz n
-}