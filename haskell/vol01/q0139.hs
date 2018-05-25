import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String
import Test.HUnit


main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  n <- getl toInt
  xs <- replicateM n getLine
  mapM_ putStrLn $ solve xs

solve :: [String] -> [String]
solve = map snake

snake :: String -> String
snake s
  | a = "A"
  | b = "B"
  | otherwise = "NA"
  where
    a = either (\_ -> False) id (parse snakeA "" s)
    b = either (\_ -> False) id (parse snakeB "" s)

snakeA :: Parser Bool
snakeA = do
  string ">'"
  a <- many1 (char '=')
  char '#'
  b <- many1 (char '=')
  char '~'
  return $ length a == length b
  
snakeB :: Parser Bool
snakeB = do
  string ">^"
  many1 (string "Q=")
  string "~~"
  return True
  
toInt :: String -> Int
toInt s = read s

getl :: (String -> a) -> IO a
getl f = f <$> getLine


tests :: [Test]
tests = [
  "test1" ~: Right True ~=? parse snakeA "" ">'======#======~",
  "test2" ~: Right False ~=? parse snakeA "" ">'======#=====~",
  "test3" ~: False ~=? either (\_ -> False) id (parse snakeA "" ">^Q=Q=Q=Q=Q=Q=Q=Q=Q=~~"),
  "test4" ~: Right True ~=? parse snakeB "" ">^Q=Q=Q=Q=Q=Q=Q=Q=Q=~~",
  "test5" ~: False ~=? either (\_ -> False) id (parse snakeB "" ">^Q=Q=Q=Q=Q=Q=Q=Q=Q=~"),
  "test6" ~: "A" ~=? snake ">'======#======~",
  "test7" ~: "B" ~=? snake ">^Q=Q=Q=Q=Q=Q=Q=Q=Q=~~",
  "test8" ~: "NA" ~=? snake ">'===#====~"
  ]
