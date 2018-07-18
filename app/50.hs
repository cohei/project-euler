import Data.List
import Data.Numbers.Primes hiding (isPrime)

con n = transpose $ take n $ tails primes
-- nが偶数のとき、2を含むものだけが素数の可能性がある(和が奇数になる)
con' n = (if even n then (:[]) . head else id) $ con n

-- length $ takeWhile (1000000>) $ scanl1 (+) primes = 546
ns = concatMap (takeWhile ((1000000>) . fst) . map (\xs -> (sum xs, xs)) . con') [22..546]
-- 全体にfilter (1000000>)はこれを越える数までチェックしてしまう

isPrime n = all ((0/=) . (n `mod`)) $ takeWhile (n'>=) primes
    where n' = floor $ sqrt $ fromIntegral n

main = print $ fst $ last $ filter (isPrime . fst) ns
