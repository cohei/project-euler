import Data.List

pan = filter ((0/=) . head) $ permutations [0..9]

ps = [2,3,5,7,11,13,17]

tup3 ns = zip3 ns (tail ns) (drop 2 ns)

num (x,y,z) = 100*x + 10*y + z

predi ns = all (0==) $ zipWith mod (map num . tail . tup3 $ ns) ps

tonum = foldl1 ((+) . (10*))

main = print $ sum $ map tonum $ filter predi pan
