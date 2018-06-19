import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Control.Monad.State

main :: IO ()
main = do
  n <- f
  unless (n == 0) $ do
    solve <$> replicateM n f >>= print
    main
  where f = readi B.readInt <$> B.getLine

solve :: [Int] -> Int
solve xs = evalState (puto xs) []

puto :: [Int] -> State [(Int, Int)] Int
puto [] = do
  ls <- get
  return $ sum $ map snd $ filter ((==0) . fst) ls
puto (x:xs) = do
  ls <- get
  case ls of
   ((c, s) : rs) | c == x -> put $ (x, s+1) : rs
   _ -> modify ((x, 1) :)
  pute xs
  
pute :: [Int] -> State [(Int, Int)] Int
pute [] = do
  ls <- get
  return $ sum $ map snd $ filter ((==0) . fst) ls
pute (x:xs) = do
  ls <- get
  case ls of
   ((c, s) : rs) | x == c -> put $ ((x, s+1) : rs)
   ((_, s1) : (_, s2) : rs) -> put $ (x, (1+s1+s2)) : rs
   [(c, s)] -> put [(x, s+1)]
  puto xs

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
