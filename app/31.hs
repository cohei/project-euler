import qualified Data.Map as M
import Data.Function
import Data.Function.Memoize

import Memo

coins :: [Int]
coins = [1,2,5,10,20,50,100,200] -- make 200

-- memoise 使おう


make 0 _  = 1
make _ [] = 0
make n ccs@(c:cs)
    | n < 0     = 0
    | otherwise = make (n-c) ccs + make n cs

makeFU _ (0,_)  = 1
makeFU _ (_,[]) = 0
makeFU f (n,ccs@(c:cs))
    | n < 0     = 0
    | otherwise = f ((n-c),ccs) + f (n,cs)

makeF' :: (Int,[Int]) -> Int
makeF' = gfun ($) makeFU

gfun = (fix .) . (.)

mmake :: Table t => Memo t (Int,[Int]) Int
mmake = gfun memoise makeFU

e :: M.Map (Int,[Int]) Int
e = emptyTable

run = curry (evalMemo mmake e)

main = print $ run 200 coins
