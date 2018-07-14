import Data.Function ( on )
import Data.List -- ( findIndex, maximumBy )
import Data.Maybe ( fromJust )

-- 合成数の逆数の循環周期はその素因数のうち最大の循環周期をもつもののものと等しいため、素数のみ考える

xs = takeWhile (<1000) primes
primes = 2 : sieve [3,5..]
    where sieve (p:ns) = p : sieve [n | n <- ns, mod n p /= 0]

divMods n m = let qr@(_,r) = divMod n m
              in qr : divMods (10*r) m

period (x:xs) = 1 + (fromJust $ findIndex (x==) xs)
period' xs = period . snd . unzip . drop 5 . dropWhile ((0==) . fst) $ xs
  -- drop 5 で循環が始まる前の数を落としている (非理論的)
main = print $ fst $ maximumBy (compare `on` snd) $ zip xs $ map (period' . divMods 1) xs
