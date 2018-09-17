import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')

main :: IO ()
main = do
  [n, k] <- map read <$> words <$> getLine
  unless (n == 0 && k == 0) $ do
    solve k <$> replicateM n (readi B.readInt <$> B.getLine) >>= print
    main

solve :: Int -> [Int] -> Int
solve k xs = fst $ foldl' f (sc, sc) $ zipWith (-) zs xs
  where (ys, zs) = splitAt k xs
        sc = sum ys
        f (sm, sc) d | sn > sm = (sn, sn)
                     | otherwise = (sm, sn)
          where sn = sc + d

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
