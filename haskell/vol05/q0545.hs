import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM, unless)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  n <- readLn
  m <- readLn
  unless (n == 0 && m == 0) $ do
    solve n <$> replicateM m (map read <$> words <$> getLine) >>= print
    main

solve :: Int -> [[Int]] -> Int
solve n xss = let s1 = al ! 1
              in max 0 . subtract 1 . IS.size $ IS.foldl' (\s i -> IS.union s (al ! i)) s1 s1
  where al = foldl' (\v [a, b] -> let v' = v // [(b, (IS.insert a (v ! b)))]
                                  in v' // [(a, (IS.insert b (v' ! a)))]
                    ) (V.replicate (n+1) IS.empty) xss
