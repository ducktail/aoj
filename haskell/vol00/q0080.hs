import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  when (n /= (-1)) $ do
    print $ solve n
    main
    
solve :: Int -> Double
solve n = thirdroot (fromIntegral n) (fromIntegral n / 2.0)

recform :: Double -> Double -> Double
recform q x = x - (x * x * x - q) / (3 * x * x)

thirdroot :: Double -> Double -> Double
thirdroot q x
  | abs (nx ^ 3 - q) < 0.00001 * q = nx
  | otherwise = thirdroot q nx
  where
    nx = recform q x

toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
