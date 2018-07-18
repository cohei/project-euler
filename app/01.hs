module Main where

main :: IO ()
main = print . sum . takeWhile (1000>) $ multsOf35

multsOf35 :: [Int]
multsOf35 = map (*3) [1..] # map (*5) [1..]
  where
    xxs@(x:xs) # yys@(y:ys)
      | x <  y = x : xs  # yys
      | x == y = x : xs  # ys
      | x >  y = y : xxs # ys
    _ # _ = error "無限リストを想定"
