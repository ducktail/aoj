import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= print
    main
  where
    f = map toSec <$> words <$> getLine

solve :: [[Int]] -> Int
solve = fst . IM.foldl' f (0, 0) . foldl' g IM.empty
  where
    f (mxc, acc) x = let x' = acc + x
                     in (max mxc x', x')
    g m [lt, at] = IM.insertWith (+) lt 1 (IM.insertWith (+) at (-1) m)

toSec :: String -> Int
toSec = sum . zipWith (*) [3600, 60, 1] . map read . splitOn ":"
