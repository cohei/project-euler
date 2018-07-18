import Data.Ratio

n x y = 10*x+y

-- xy/xz はありえない xy/zy はy=0のtrivialしかない
-- xy/yz 
xyyz = [ (show (n x y) ++ "/" ++ show (n y z), x%z)
       | x <- [1..9], y <- [1..9], z <- [1..9]
       , n x y < n y z, n x y % n y z == x % z ]
-- xy/zx
xyzx = [ (show (n x y) ++ "/" ++ show (n z x), y%z)
       | x <- [1..9], y <- [1..0], z <- [1..9]
       , n x y < n z x, n x y % n z x == y % z ]

main = print $ denominator $ product $ map snd $ xyyz ++ xyzx
