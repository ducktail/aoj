import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOneOf)
import Data.Char (intToDigit)
import Test.HUnit

main :: IO ()
-- main = runTestTT tests >> return ()
main = do
  xs <- getc $ solve . splitOneOf "+="
  mapM_ printAnswer xs
  
solve :: [String] -> Maybe Int
solve [s1,s2,s3]
  | null rs = Nothing
  | otherwise = Just $ head rs
  where
    ls = if (f s1) || (f s2) || (f s3) then [1..9] else [0..9]
    f s = length s /= 1 && head s == 'X'
    rs = filter (isCorrect s1 s2 s3) ls
    
toNum :: String -> Int -> String
toNum ss i = map (\c -> if c == 'X' then (intToDigit i) else c) ss

isCorrect :: String -> String -> String -> Int -> Bool
isCorrect s1 s2 s3 n = i1 + i2 == i3
  where
    i1 = toInt $ toNum s1 n
    i2 = toInt $ toNum s2 n
    i3 = toInt $ toNum s3 n

printAnswer :: Maybe Int -> IO ()
printAnswer Nothing = putStrLn "NA"
printAnswer (Just x) = print x

toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents

tests = TestList [ "toNum1" ~: "123" ~=? toNum "12X" 3,
                   "toNum2" ~: "939495" ~=? toNum "X3X4X5" 9,
                   "toNum3" ~: "890820" ~=? toNum "89X82X" 0,
                   "isCorrect1" ~: True ~=? isCorrect "123X" "3X56" "X690" 4,
                   "isCorrect2" ~: False ~=? isCorrect "5X" "X7X9" "XX4X" 8,
                   "rs1" ~: [6] ~=? filter (isCorrect "1234" "5X78" "X912") [0..9],
                   "solve1" ~: Just 4 ~=? solve ["123X","3X56","X690"],
                   "solve2" ~: Nothing ~=? solve ["123X", "3X56", "X69X"],
                   "solve3" ~: Just 0 ~=? solve ["X", "1XX", "1XX"] ]
