import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    ((unwords . map show) . solve) <$> replicateM n (getl $ wrds toInt) >>= putStrLn
    main

solve :: [[Int]] -> [Int]
solve = g . maximumBy (compare `on` snd) . map f
  where
    f [x,y,z] = (x, (y + z))
    g (x,y) = [x, y]
    
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
