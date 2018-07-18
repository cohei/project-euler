
tris = map (\n -> n * (n+1) `div` 2) [1..]
pens = map (\n -> n * (3*n-1) `div` 2) [1..]
hexs = map (\n -> n * (2*n-1)) [1..]

combine xxs@(x:xs) yys@(y:ys)
    | x < y     = x : combine xs yys
    | otherwise = y : combine xxs ys

same3 xs = map (\(x,_,_) -> x) $ 
           filter (\(x,y,z) -> x == y && x == z) $ zip3 (drop 2 xs) (tail xs) xs

main = print $ head $ dropWhile (<=40755) $ same3 $ combine tris $ combine pens hexs
