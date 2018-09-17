import Control.Applicative ((<$>))
import Control.Monad (unless, replicateM)

import Data.List (foldl', group, sort, sortBy, intercalate)
import Data.Function (on)

import Data.Map (Map)
import qualified Data.Map as M


main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n >>= putStrLn
    main
    
solve :: Int -> IO String
solve n = do
  ss <- replicateM n (words <$> getLine)
  k <- head <$> getLine
  case M.lookup k (foldl' f M.empty (concat ss)) of
   Nothing -> return "NA"
   Just ls -> return . intercalate " " . map snd . take 5 . sortBy g . map (\x -> (length x, head x)) . group . sort $ ls
  where f m s = M.insertWith (++) (head s) [s] m
        g (l1, s1) (l2, s2) | l1 == l2 = compare s1 s2
                            | otherwise = compare l2 l1
