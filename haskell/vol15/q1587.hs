import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [w, h, _] <- f
  p <- readLn
  solve w h <$> replicateM p f <*> replicateM h f >>= print
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [[Int]] -> [[Int]] -> Int
solve w h pss sss = sum $ zipWith (*) pl sl
  where
    idx x y = x + y * w
    f v [x,y,_] = V.accum (+) v [(idx x y, 1)]
    pl = V.toList $ foldl' f (V.replicate (w*h) 0) pss
    sl = concat sss
