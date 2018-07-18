import Data.List

largest :: Int
largest = 999 * 999

sixDigitParindrome0 :: [Int]
sixDigitParindrome0 =
  [10^5*p + 10^4*q + 10^3*r + 10^2*r + 10*q + p | p <- [9,8..1], q <- [9,8..0], r <- [9,8..0]]

sixDigitParindrome :: [Int]
sixDigitParindrome = dropWhile (largest<) sixDigitParindrome0

factorPairs n = [(x,n`div`x) | x <- [100..k], n `mod` x == 0]
    where k = let n1 = round $ sqrt $ fromIntegral n
              in if n1 > 100 then n1 else 100

threeDigit n = 100 <= n && n <= 999

main = print . uncurry (*) . head . dropWhile (not . threeDigit . snd) . concat . map factorPairs $ sixDigitParindrome
