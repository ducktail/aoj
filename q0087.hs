import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Control.Monad.State

main :: IO ()
main = getc (solve . words) >>= mapM_ print

solve :: [String] -> Double
solve xs = evalState (calc xs) []

calc :: [String] -> State [Double] Double
calc [] = do
  (x:xs) <- get
  return x

calc (x:xs)
  | x == "+" = do
      (a:b:cs) <- get
      put (a+b:cs)
      calc xs
  | x == "-" = do
      (a:b:cs) <- get
      put (b-a:cs)
      calc xs
  | x == "*" = do
      (a:b:cs) <- get
      put (a*b:cs)
      calc xs
  | x == "/" = do
      (a:b:cs) <- get
      put (b/a:cs)
      calc xs
  | otherwise = do
      cs <- get
      put (toDbl x : cs)
      calc xs
  
toDbl :: String -> Double
toDbl s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
