import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == (-1)) $ do
    print . solve $ n
    main
    
solve :: Int -> Int
solve n = 4280 - (bill n)
  where
    bill n
      | n <= 10 = 1150
      | n <= 20 = 1150 + 125 * (n - 10)
      | n <= 30 = 2400 + 140 * (n - 20)
      | otherwise = 3800 + 160 * (n - 30)

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
