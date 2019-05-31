import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)

main :: IO ()
main = do
  q <- readLn
  solve <$> replicateM q f >>= mapM_ print
  where
    f = readil B.readInt <$> B.getLine

solve :: [[Int]] -> [Int]
solve = toListZ . foldl' f emptyZ
  where
    f z [0, x] = insertZ z x
    f z [1, d] = moveZ z d
    f z [2] = eraseZ z

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ \s -> do
  (n, s') <- f s
  return (n, B.dropWhile isSpace s')

type Zipper a = ([a], [a])

emptyZ :: Zipper Int
emptyZ = ([], [])

insertZ :: Zipper Int -> Int -> Zipper Int
insertZ (as, bs) x = (as, x:bs)

moveZ :: Zipper Int -> Int -> Zipper Int
moveZ z d = f z d
  where
    f (as, bs) d
      | d == 0 = (as, bs)
      | d > 0 = case bs of
                 (b:bs) -> f (b:as, bs) (d - 1)
                 [] -> (as, [])
      | otherwise = case as of
                     (a:as) -> f (as, a:bs) (d + 1)
                     [] -> ([], bs)

eraseZ :: Zipper Int -> Zipper Int
eraseZ (as, bs) = case bs of
                   [] -> (as, [])
                   (b:bs) -> (as, bs)

toListZ :: Zipper Int -> [Int]
toListZ (as, bs) = reverse as ++ bs
