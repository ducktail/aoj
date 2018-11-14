import Control.Applicative
import Control.Monad
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  [_, k] <- f
  bs <- replicateM k (IS.fromList <$> tail <$> f)
  r <- readLn
  solve bs <$> replicateM r f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [IntSet] -> [[Int]] -> Int
solve bs es = IS.size $ foldl' f IS.empty bs
  where
    f st b = foldl' (g b) st es
    g b st [x, y]
      | IS.member x b && IS.member y b = IS.insert x (IS.insert y st)
      | otherwise = st
