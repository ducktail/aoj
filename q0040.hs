import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (ord, chr)

main = do
  n <- readLn :: IO Int
  xs <- replicateM n getLine
  mapM_ putStrLn $ solve xs
 
solve :: [String] -> [String]
solve = map dcrpts
 
itoa :: Int -> Char
itoa x = chr $ ord 'a' + x
 
atoi :: Char -> Int
atoi x = ord x - ord 'a'
 
ac :: (Int,Int) -> Int -> Int
ac (a,b) y 
  | y >= 0 && y <= 25 = (a * y + b ) `mod` 26
  | otherwise = y
 
cnds :: [(Int,Int)]
cnds = [(x,y) | x <- [1,3,5,7,9,11,15,17,19,21,23,25], y <- [0..25]]
 
acs :: [Int -> Int]
acs = map ac cnds
 
dcrpt :: [Int] -> [[Int]]
dcrpt xs = do
  f <- acs
  return $ map f xs
 
dcrpts :: String -> String
dcrpts = head . filter (\x -> "this" `isInfixOf` x || "that" `isInfixOf` x) . map (map itoa) . dcrpt . map atoi
