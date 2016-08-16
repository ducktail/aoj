import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  k <- getl toInt
  unless (k == 0) $ do
    solve k <$> getl (wrds toInt) >>= print
    main

solve :: Int -> [Int] -> Int
solve k xs = sum xs `div` (k-1)

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
