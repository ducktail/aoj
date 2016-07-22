import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  x <- getl toInt
  h <- getl toInt
  when (x /= 0 || h /= 0) $ do
    print $ solve x h
    main
    
solve :: Int -> Int -> Double
solve x h = as + at4
  where
    as = (fromIntegral x) ^ 2
    at4 = 2.0 * (fromIntegral x) * l
    l = sqrt $ (fromIntegral h) ^ 2 + (fromIntegral x) ^ 2 / 4.0
    
toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
