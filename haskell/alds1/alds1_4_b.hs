import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Data.Char (isSpace)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

main :: IO ()
main = solve <$> readLn <*> f <*> (B.getLine >> f) >>= print
  where f = readiv B.readInt <$> B.getLine

solve :: Int -> Vector Int -> Vector Int -> Int
solve n ss ts = V.foldl' f 0 ts
  where f ct x | bsearch (flip compare x . (ss !)) 0 n = ct + 1
               | otherwise = ct
  

bsearch :: (Int -> Ordering) -> Int -> Int -> Bool
bsearch f l r | l >= r = False
              | otherwise = case f m of
                             GT -> bsearch f l m
                             LT -> bsearch f (m+1) r
                             EQ -> True
  where
    m = (l + r) `div` 2

readiv :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> Vector a
readiv f = V.unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
