[wimport Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (sort, delete)

main :: IO ()
main = do
  n <- readi B.readInt <$> B.getLine
  solve <$> replicateM n (readi B.readInt <$> B.getLine) >>= print

solve :: [Int] -> Int
solve xs = (!!2) . sort . f . take 4 . sort $ xs
  where f :: [Int] -> [Int]
        f xs = do
          x <- xs
          y <- delete x xs
          return . read $ show x ++ show y

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
]
