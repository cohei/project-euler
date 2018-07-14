-- ghciで:set -i..を実行
import Memo
import qualified Data.Map as M
import Data.Function

fibfix f x = case x of
               0 -> 0
               1 -> 1
               _ -> f (x-1) + f (x-2)

gfun x y = fix $ x . y

mfib = gfun memoise fibfix

memofib = evalMemo mfib M.empty

main = print $ fst $ head $ dropWhile ((1000 >) . length . show . snd) $ zip [1..] $ map memofib [1..]