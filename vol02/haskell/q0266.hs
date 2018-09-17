import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  ps <- getLine
  unless (ps == "#") $ do
    putStrLn $ solve ps
    main

solve :: String -> String
solve = g . (== 'B') . (foldl f 'A')
  where
    f 'D' _ = 'D'
    f 'A' '0' = 'X'
    f 'A' '1' = 'Y'
    f 'X' '0' = 'D'
    f 'X' '1' = 'Z'
    f 'Y' '0' = 'X'
    f 'Y' '1' = 'D'
    f 'Z' '0' = 'W'
    f 'Z' '1' = 'B'
    f 'W' '0' = 'B'
    f 'W' '1' = 'Y'
    f 'B' '0' = 'Y'
    f 'B' '1' = 'X'
    g True = "Yes"
    g False = "No"
