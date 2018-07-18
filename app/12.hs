
--triangleNumbers = scanl1 (+) [1,2..]

numberOfFactors :: Integer -> Int
numberOfFactors n | n `mod` m == 0 = 2 * l + 1
                  | otherwise      = 2 * l
    where m = floor $ sqrt $ fromInteger n
          l = length [f | f <- [1..m-1], n `mod` f == 0]

series1 = map (\x -> if even x then div x 2 else x) [1..]
-- [1, 2/2, 3, 4/2, 5, 6/2, 7, ..]
series2 = zip series1 $ map numberOfFactors series1
series3 = zipWith (\(x,y) (z,w) -> ((x,z),y*w)) series2 $ tail series2

main :: IO ()
main = print $ uncurry (*) $ fst $ head $ dropWhile ((500>=) . snd) series3
