import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  map solve <$> replicateM n (getLine >> f) >>= mapM_ putStrLn
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> String
solve xs = g . foldl' f (0, 0) $ zip xs (tail xs)
  where
    f (mxu, mxd) (a, b) = (max mxu (b - a), max mxd (a - b))
    g (a, b) = unwords . map show $ [a, b]
