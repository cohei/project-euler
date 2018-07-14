-- Evaluate the sum of all amicable pairs under 10000.

main :: IO ()
main = print $ sum $ filter isAmicable [2..10000]

factors :: Int -> [Int]
factors n = filter ((0==) . (mod n)) [1..n]

factorSum :: Int -> Int
factorSum = sum . init . factors

isAmicable :: Int -> Bool
isAmicable n = let m = factorSum n
               in n /= m && n == factorSum m