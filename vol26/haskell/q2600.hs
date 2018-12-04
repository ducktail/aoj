import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool (bool)

main :: IO ()
main = do
  [n, w, h] <- f
  solve w h <$> replicateM n (g <$> f) >>= putStrLn
  where
    f = map read <$> words <$> getLine
    g [x, y, w] = ((x - w, x + w), (y - w, y + w))

solve :: Int -> Int -> [((Int, Int), (Int, Int))] -> String
solve w h ps = let xs = sort . map fst $ ps
                   ys = sort . map snd $ ps
               in bool "No" "Yes" $ f w 0 xs || f h 0 ys
  where
    f mx cp as
      | cp >= mx = True
      | null as = False
      | cp < up = False
      | vp < cp = f mx cp rs
      | otherwise = f mx vp rs
      where
        ((up, vp):rs) = as
