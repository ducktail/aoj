import Control.Applicative ((<$>))
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = getLine >> solve <$> map read <$> words <$> getLine >>= print

solve :: [Int] -> Int
solve (x:xs) = f ((V.replicate 21 0) // [(x,1)]) xs
  where f :: Vector Int -> [Int] -> Int
        f v [y] = v ! y
        f v (y:ys) = f nv ys
          where nv = foldl' g (V.replicate 21 0) [0..20]
                g :: Vector Int -> Int -> Vector Int
                g v' i = v' // [(i, (if i-y>=0 then (v ! (i-y)) else 0) + (if i+y<=20 then (v ! (i+y)) else 0))]
