import Data.IntSet ( elems, fromList )

factors :: Int -> [Int]
factors n = filter ((0==) . mod n) [1..n]

factors2 :: Int -> [Int] -- 足すから、約数の順番関係ない n自身はいらない
factors2 n = let sn = sqrt $ fromIntegral n
                 fsn = floor sn
                 fhalf = filter ((0==) . mod n) [fsn,(fsn-1)..2]
                 rhalf = map (div n) fhalf
             in 1 : if null fhalf
                    then []
                    else if head fhalf == head rhalf 
                         then tail fhalf ++ rhalf
                         else fhalf ++ rhalf

isAbundant :: Int -> Bool
isAbundant n = n < sum (factors2 n)

upper = 28123
upper2 = 14060 -- the largest abundant number which is less than the half of upper

abundants1 = filter isAbundant [1..14060]
abundants2 = filter isAbundant [14061..upper]
abundants = abundants1 ++ abundants2

-- diabundants db

dbRow1 n = map (n+) $ takeWhile (n>=) abundants1
dbRow2 n = takeWhile (upper >=) $ map (n+) $ abundants1

dbTotal = concat $ map dbRow1 abundants1 ++ map dbRow2 abundants2

main = print $ sum [1..upper] - sum (elems $ fromList dbTotal)
