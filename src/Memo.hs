{-# language FlexibleInstances, TypeSynonymInstances #-}
module Memo where

import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M

class Table t where
    emptyTable  :: Ord a => t a b
    lookupTable :: Ord a => a -> t a b -> Maybe b
    insertTable :: Ord a => a -> b -> t a b -> t a b

instance Table M.Map where
    emptyTable = M.empty
    lookupTable = M.lookup
    insertTable = M.insert

newtype AList a b = A { unA :: [(a,b)] }

instance Table AList where
    emptyTable = A []
    lookupTable = (. unA) . L.lookup
    insertTable = ((. unA) .) . ((A .) .) . ((:) .) . (,)

instance (Table t, Ord a, Num b, Eq b) => Eq (State (t a b) b) where
    s1 == s2 = evalState s1 emptyTable == evalState s2 emptyTable

instance (Table t, Ord a, Num b, Show b) => Show (State (t a b) b) where
    show sx = show (evalState sx emptyTable)

instance (Table t, Ord a, Num b) => Num (State (t a b) b) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum
    fromInteger = return . fromInteger

type Memo t a b = a -> State (t a b) b

memoise :: (Table t, Ord a) => Memo t a b -> Memo t a b
memoise mf x = do prev <- find x
                  case prev of
                    Just y -> return y
                    Nothing -> do y <- mf x
                                  ins x y
                                  return y
    where find x = get >>= return . lookupTable x
          ins x y = get >>= put . insertTable x y

runMemo :: (Table t, Ord a) => Memo t a b -> t a b -> a -> (b, t a b)
runMemo m t v = runState (m v) t

evalMemo :: (Table t, Ord a) => Memo t a b -> t a b -> a -> b
evalMemo m t v = evalState (m v) t

execMemo :: (Table t, Ord a) => Memo t a b -> t a b -> a -> t a b
execMemo m t v = execState (m v) t

memoMap :: (Table t, Ord a) => t a b -> Memo t a b -> [a] -> [b]
memoMap e mf xs = snd $ L.mapAccumL ((swap .) . (flip (runState . mf))) e xs
    where swap (x,y) = (y,x)
