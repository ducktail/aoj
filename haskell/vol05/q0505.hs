import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (transpose, sortBy)

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    solve <$> replicateM n f >>= putStrLn
    main
  where f = map read <$> words <$> getLine
  
solve :: [[Int]] -> String
solve = unwords . map (show . fst) . sortBy f . zip [1..] . map sum . transpose
  where f (a, b) (c, d) | b == d = compare a c
                        | otherwise = compare d b
