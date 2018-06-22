import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (sortBy, unfoldr)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    mapM_ putStrLn $ solve n 
    main

solve :: Int -> [String]
solve n = showSquare $ (S.singleton [] : unfoldr f (1, S.singleton [1])) !! n
  where f (i,st) = Just (st, (i+1, S.foldl' g (S.singleton (replicate (i+1) 1)) st))
          where g s l = S.union s (S.fromList (take (length l) (zipWith (\l1 l2 -> sort (zipWith (+) l1 l2)) (ohs i) (repeat l))))

showSquare :: Set [Int] -> [String]
showSquare st = S.foldl' f [] st
  where f ls x = (unwords . map show) x : ls

sort = sortBy (flip compare)

ohs :: Int -> [[Int]]
ohs n = take n (map (take n) (iterate (0:) (1:repeat 0)))
