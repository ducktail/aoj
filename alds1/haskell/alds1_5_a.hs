import Control.Applicative ((<$>), (<*>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bool (bool)
import Data.List (unfoldr)
import Data.Char (isSpace)

main :: IO ()
main = solve <$> f <*> f >>= mapM_ putStrLn
  where f = B.getLine >> readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> [String]
solve as = map (bool "no" "yes" . search as)

search xs m | m == 0 = True
            | m < 0 = False
            | null xs = False
            | otherwise = let (y:ys) = xs
                          in search ys m || search ys (m - y)

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
