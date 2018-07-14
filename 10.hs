import Control.Applicative ( (<*>) )
{-
s f g x = f x (g x)
s == (<*>)
(<*>) :: Applicative a => a (b -> c) -> a b -> a c
今はa = (->) a
(<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)

\n p -> n >= p*p
\n p -> n >= ((*) <*> id) p
 infixl 4 <*>, >=
\n p -> (n >=) $ ((*) <*> id) p
\n -> (n >=) . ((*) <*> id)

\n p -> n `mod` p /= 0
\n p -> (0 /=) $ mod n p
(0 /=) . mod
-}
prime :: Integer -> Bool
prime n = all ((0 /=) . mod n) $ takeWhile ((n >=) . ((*) <*> id)) primes

primes :: [Integer]
primes = 2 : filter prime [3,5..]

main = print . sum . takeWhile (2000000>) $ primes