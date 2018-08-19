import Control.Applicative
import Control.Monad
import Data.List
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, k, s] <- map read <$> words <$> getLine
  unless (n == 0 && k == 0 && s == 0) $ do
    print $ solve n k s
    main

solve :: Int -> Int -> Int -> Int
solve n k s = dp ! (idx (k, s))
  where idx (i, j) = i*(s+1)+j
        iv = V.replicate ((k+1)*(s+1)) 0 // [(0,1)] :: Vector Int
        dp = foldl' f iv [1..n]
        f v x = V.accum (+) v $ do
          i <- [0..k-1]
          j <- [0..s]
          let y = v ! (idx (i,j))
          guard $ y > 0
          guard $ j + x <= s
          return (idx (i+1, j+x), y)
