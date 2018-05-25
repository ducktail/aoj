import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Data.Char (isSpace)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve n <$> readil B.readInt <$> B.getLine >>= print
    main
    
solve :: Int -> [Int] -> Int
solve n xs = cnt 0 (n, xs)
  where f (c, as) = foldr g (1, [c]) as
          where g e (d, bs) | e == 1 = (d, bs)
                            | otherwise = (d+1, (e-1):bs)
        cnt i (c, as) | i > 10000 = (-1)
                      | isTriangle as = i
                      | otherwise = cnt (i+1) (f (c, as))

isTriangle :: [Int] -> Bool
isTriangle xs = head xs == 1 && all f (zip xs (tail xs))
  where f (a, b) = a + 1 == b
        
readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
