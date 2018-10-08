import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  replicateM_ n $ do
    solve <$> f <*> f >>= print
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> [Int] -> Int
solve [_, k] = sum . drop (k - 1) . sortBy (flip compare) . (tail >>= zipWith (-))
