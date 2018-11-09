import Control.Applicative
import Control.Monad
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f <*> f >>= print
    main
  where
    f = IS.fromList <$> map read <$> tail <$> words <$> getLine

solve :: [IntSet] -> IntSet -> Int
solve cds lk = let cdd = filter f $ zip cds [1..]
               in case cdd of
                   [(_,i)] -> i
                   _ -> (-1)
  where
    f (x, _) = IS.intersection x lk == lk
