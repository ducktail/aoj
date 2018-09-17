import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = solve <$> (map read <$> words <$> getLine) <*> (map (readi B.readInt) <$> B.lines <$> B.getContents) >>= print

solve :: [Int] -> [Int] -> Int
solve [_, k] ws = bsearch (f 0 0 ws) 0 (sum ws)
  where f ct sw [] p = ct + 1 <= k
        f ct sw (x:xs) p | ct > k = False
                         | sw + x > p = f (ct + 1) 0 (x:xs) p
                         | otherwise = f ct (sw + x) xs p

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f fp pp | fp + 1 == pp = pp
                | f tp = bsearch f fp tp
                | otherwise = bsearch f tp pp
  where tp = (fp + pp) `div` 2

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
