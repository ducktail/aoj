import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == (-1)) $ do
    putStrLn $ solve n
    main

solve :: Int -> String
solve = g . reverse . unfoldr f
  where
    f x = if x == 0 then Nothing else Just (x `mod` 4, x `div` 4)
    g x = if null x then "0" else concatMap show x
            
toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine
