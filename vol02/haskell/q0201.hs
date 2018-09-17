import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List (foldl')
import Data.Map (Map, (!))
import qualified Data.Map as M

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
  ps <- replicateM n (words <$> getLine)
  m <- readLn
  rs <- replicateM m (words <$> getLine)
  t <- getLine
  solve ps rs t >>= print
  main
  
solve :: [[String]] -> [[String]] -> String -> IO Int
solve ps rs t = return $ price rtbl ptbl t
  where
    ptbl = M.fromList $ map (\[x, y] -> (x, P (read y))) ps
    rtbl = M.fromList $ map (\(x:_:xs) -> (x, xs)) rs

price :: Map String [String] -> Map String Price -> String -> Int
price rt pt t = fst $ f pt t
  where f :: Map String Price -> String -> (Int, Map String Price)
        f pt t = case (pt ! t) of
                  F x -> (x, pt)
                  P x -> case M.lookup t rt of
                          Just rs -> let (y, pt') = foldl' g (0, pt) rs
                                     in if x < y then (x, M.insert t (F x) pt') else (y, M.insert t (F y) pt')
                          Nothing -> (x, M.insert t (F x) pt)
        g (s, pt) t = let (y, pt') = f pt t in (s+y, pt')
  
data Price = P Int | F Int deriving Show
