import Control.Applicative
import Data.List

main :: IO ()
main = solve <$> readLn >>= print

solve :: Int -> Int
solve n = minimumBy f ps
  where
    ps = filter isPal [1..9999]
    f a b = compare (abs (a - n)) (abs (b - n))
isPal :: Int -> Bool
isPal n = let ns = unfoldr f n
          in (reverse >>= (==)) ns
  where
    f 0 = Nothing
    f x = Just $ let (q, r) = divMod x 10 in (r, q)
