import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, l] <- map read <$> words <$> getLine
  solve n l <$> replicateM n (readi B.readInt <$> B.getLine) >>= print

solve :: Int -> Int -> [Int] -> Int
solve n l xs = V.maximum $ foldl' (\t i -> fst $ f t i) tm [1..n]
  where icv = V.fromList $ 0 : (xs ++ [0])
        tm = V.replicate (n+1) 0
        f t i | t ! i > 0 = (t, t ! i)
              | lc > lp && lc > ln = (t // [(i, l - lc)], l - lc)
              | lc < lp && lc < ln = let (t1, x) = f t (i-1)
                                         (t2, y) = f t1 (i+1)
                                         z = (l - lc) + max x y
                                     in (t2 // [(i, z)], z)
              | lc < lp = let (t1, x) = f t (i-1)
                              z = (l - lc) + x
                          in (t1 // [(i, z)], z)
              | otherwise = let (t1, x) = f t (i+1)
                                z = (l - lc) + x
                                in (t1 // [(i, z)], z)
          where lc = icv ! i
                lp = icv ! (i-1)
                ln = icv ! (i+1)

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
