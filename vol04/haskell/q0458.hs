import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  [n, c] <- f
  solve c <$> replicateM n readLn <*> replicateM n readLn <*> replicateM n readLn >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> [Int] -> [Int] -> [Int] -> Int
solve c as bs xs = f 1
  where
    f k = let ck = foldl3 (\cc a b x -> cc + a * (max 0 (k * x - b))) 0 as bs xs in
      if c < ck then (k - 1) * (sum xs) else f (k + 1)

foldl3 f x [] _ _ = x
foldl3 f x _ [] _ = x
foldl3 f x _ _ [] = x
foldl3 f x (a:as) (b:bs) (c:cs) = foldl3 f (f x a b c) as bs cs
