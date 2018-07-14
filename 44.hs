import Control.Monad

pen 0 = 0
pen n = pen (n-1) + 3*n-2

pens = map pen [1..]

satisfied = do 
  p1 <- pens
  let pens1 = takeWhile (p1>) pens
  p2 <- pens1
  let pens2 = takeWhile (p1*2>) pens
  guard $ p1 - p2 `elem` pens1
  guard $ p1 + p2 `elem` pens2
  return $ p1 - p2

main = print $ head $ satisfied -- 厳密にはheadじゃないかもしれないが、正解
