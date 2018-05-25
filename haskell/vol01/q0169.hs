import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (foldl')

main :: IO ()
main = do
  cs <- map read <$> words <$> getLine
  unless (cs == [0]) $ do
    solve cs >>= print
    main

solve :: [Int] -> IO Int
solve = return . maximum . (0:) . foldl' f [0]
  where f xs y = [w | u <- xs, v <- g y, let w = u + v, w <= 21]
        g x | x == 1 = [1, 11]
            | x >= 10 = [10]
            | otherwise = [x]
