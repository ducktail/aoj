import Control.Applicative
import Control.Monad
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  n <- readLn
  map solve <$> replicateM n readLn >>= mapM_ print

solve :: Int -> Int
solve x = g 0 IS.empty x
  where
    f (x, d) = let (q, r) = divMod x d
               in if q == 0
                  then Nothing
                  else Just (q * r, (x, d * 10))
    g ct st x
      | IS.member x st = (-1)
      | x < 10 = ct
      | otherwise = g (ct + 1) (IS.insert x st) (maximum . unfoldr f $ (x, 10))
