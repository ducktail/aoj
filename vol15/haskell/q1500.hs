import Control.Applicative
import Control.Monad
import Data.List
import Data.Char (digitToInt)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = solve <$> readLn <*> getLine <*> (getLine >> f) >>= print
  where f = map read <$> words <$> getLine

solve :: Int -> String -> [Int] -> Int
solve n ids cs = dp ! ((10 - sm) `mod` 10)
  where (_,sm,as) = foldl' f (n, 0, []) ids
        f (i,s,xs) c | c == '*' && odd i = (i-1, s, cs : xs)
                     | c == '*' = (i-1, s, cs' : xs)
                     | odd i = (i-1, (s + digitToInt c) `mod` 10, xs)
                     | otherwise = (i-1, (s + dbl (digitToInt c)) `mod` 10, xs)
        cs' = map dbl cs
        dbl x = let x2 = x * 2 in if x2 > 9 then x2 - 9 else x2
        dp = foldl' g (V.replicate 10 0 // [(0,1)]) as :: Vector Int
        g v xs = V.accum (+) (V.replicate 10 0) $ do
          i <- [0..9]
          guard $ v ! i > 0
          x <- xs
          return ((i+x) `mod` 10, v ! i)
