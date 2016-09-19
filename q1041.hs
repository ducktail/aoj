import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM (n `div` 4) (getl toInt) >>= print
    main

solve :: [Int] -> Int
solve = sum

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
