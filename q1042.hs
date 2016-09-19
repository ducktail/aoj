import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOn)

main :: IO ()
main = do
  s <- getLine
  unless (s == "END OF INPUT") $ do
    putStrLn . solve $ s
    main

solve :: String -> String
solve = concatMap (show . length) . splitOn " "

