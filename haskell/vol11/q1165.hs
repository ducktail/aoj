import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (unfoldr)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM (n-1) (map read <$> words <$> getLine) >>= putStrLn
    main

solve :: [[Int]] -> String
solve xs = unwords . map show $ [maxx ls - minx ls + 1, maxy ls - miny ls + 1]
  where ls = (0,0) : unfoldr f xs
        f [] = Nothing
        f ([n, d]:rs) = Just (place (ls !! n) d, rs)

place (x, y) d | d == 0 = (x-1, y)
               | d == 1 = (x, y-1)
               | d == 2 = (x+1, y)
               | otherwise = (x, y+1)
                             
maxx = maximum . map fst
maxy = maximum . map snd
minx = minimum . map fst
miny = minimum . map snd
