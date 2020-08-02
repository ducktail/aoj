import Control.Applicative
import Data.List

main :: IO ()
main = solve <$> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> Int
solve [a, n, m] = length . filter g . takeWhile ((<= m) . snd) $ map f [1..]
  where
    f y = (y, (y + a) ^ n)
    g (y, x) = (== y) . sum $ unfoldr h x
    h a = if a == 0 then Nothing else let (q, r) = divMod a 10 in Just (r, q)
