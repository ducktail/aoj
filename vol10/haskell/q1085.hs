import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, guard)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n, s] <- map read <$> words <$> getLine
  unless (n == 0 && s == 0) $ do
    solve s <$> replicateM n (readi B.readInt <$> B.getLine) >>= print
    main

solve :: Int -> [Int] -> Int
solve s xs = s1 + s2
  where c = foldl' (\v x -> V.accum (+) v [(x,1)]) (V.replicate 101 0) xs :: Vector Int
        s1 = sum . map (\i -> let ci = (c ! i)  in ci * (ci-1) `div` 2) $ [(s `div` 2 + 1) ..100]
        s2 = sum $ do
          i <- [1..99]
          j <- [i+1 .. 100]
          guard $ i + j > s
          return $ (c ! i) * (c ! j)

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
