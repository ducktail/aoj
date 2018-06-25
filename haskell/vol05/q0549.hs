import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, m] <- map read <$> words <$> getLine
  solve <$> replicateM (n-1) f <*> replicateM m f >>= print
  where f = readi B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> Int
solve ds = snd . foldl' g (0,0)
  where g (p, ma) a = (p+a, (ma + abs ((dv ! p) - (dv ! (p+a)))) `mod` 100000)
        dv = V.unfoldr f (0, ds)
          where f ((-1), []) = Nothing
                f (x, []) = Just (x, (-1, []))
                f (x, (y:ys)) = Just (x, (x+y, ys))

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
