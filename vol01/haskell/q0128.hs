import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Test.HUnit

main :: IO ()
-- main = runTestTT (TestList tests) >> return ()
main = do
  xs <- getc solve
  mapM_ putStrLn . concat . intersperse [""] $ xs

solve :: String -> [String]
solve = transpose . map tr . fill0

fill0 :: String -> String
fill0 s = replicate (5 - length s) '0' ++ s

tr :: Char -> String
tr c
  | c == '0' = "* = ****"
  | c == '1' = "* =* ***"
  | c == '2' = "* =** **"
  | c == '3' = "* =*** *"
  | c == '4' = "* =**** "
  | c == '5' = " *= ****"
  | c == '6' = " *=* ***"
  | c == '7' = " *=** **"
  | c == '8' = " *=*** *"
  | c == '9' = " *=**** "

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents


tests :: [Test]
tests = [
  "fill01" ~: "00001" ~=? fill0 "1",
  "fill02" ~: "10201" ~=? fill0 "10201",
  "solve1" ~: ["** **","  *  ","=====","  ***","*** *","*****","** **","**** "] ~=? solve "814"
  ]
