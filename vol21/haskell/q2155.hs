import Control.Applicative
import Control.Monad
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve <$> replicateM m f >>= print
    main
  where
    f = map read <$> words <$> getLine

solve :: [[Int]] -> Int
solve =  IS.size . foldl' f (IS.singleton 1) . sort
  where
    f st [_, s, d]
      | IS.member s st = IS.insert d st
      | otherwise = st
