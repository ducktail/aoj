import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    y <- getl toInt
    solve y <$> replicateM n (getl $ wrds toInt) >>= print
    main

solve :: Int -> [[Int]] -> Int
solve y = fst . maximumBy (compare `on` snd) . map interest
  where
    interest :: [Int] -> (Int, Double)
    interest [b, r, t]
      | t == 1 = (b, 1.0 + fromIntegral y * (fromIntegral r / 100.0))
      | otherwise = (b, (1.0 + fromIntegral r / 100.0) ^ y)
                    
toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
