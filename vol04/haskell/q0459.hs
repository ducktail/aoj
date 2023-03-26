import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [n, c] <- f
  solve c <$> replicateM (fromIntegral n) readLn <*> replicateM (fromIntegral n) readLn <*> replicateM (fromIntegral n) readLn >>= print
  where
    f = map read <$> words <$> getLine

solve :: Integer -> [Integer] -> [Integer] -> [Integer] -> Integer
solve c as bs xs = (f 0 10000000000) * (sum xs)
  where
    f pk fk =
      if fk - pk <= 1 then pk
      else
        let mk = (pk + fk) `div` 2
            cc = foldl3 (\tc a b x -> tc + a * (max 0 (mk * x - b))) 0 as bs xs
        in if cc <= c then f mk fk
           else f pk mk

foldl3 f x [] _ _ = x
foldl3 f x _ [] _ = x
foldl3 f x _ _ [] = x
foldl3 f x (a:as) (b:bs) (c:cs) = foldl3 f (f x a b c) as bs cs
