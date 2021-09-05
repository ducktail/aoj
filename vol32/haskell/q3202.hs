import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.Vector.Unboxed.Mutable (IOVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [w, h, n, d, b] <- f
  unless (w == 0 && h == 0 && n == 0 && d == 0 && b == 0) $ do
    solve w h d b <$> replicateM n f >>= print
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: Int -> Int -> Int -> Int -> [[Int]] -> Int
solve w h d b ps = runST $ do
  vuf <- createUF (w * h)
  vf <- VM.replicate (w * h) False :: ST s (STVector s Bool)
  forM_ ps $ \p -> do
    writeBm vf w p
    forM_ (nbr w h d p) $ \np -> do
      t <- isBm vf w np
      when t $ do
        uniteUF vuf (toIdx w p) (toIdx w np)
  lengthUF vuf (toIdx w (ps !! (b - 1)))

toIdx :: Int -> [Int] -> Int
toIdx w [x, y] = (y - 1) * w + x - 1

writeBm :: STVector s Bool -> Int -> [Int] -> ST s ()
writeBm v w ps = VM.write v (toIdx w ps) True

isBm :: STVector s Bool -> Int -> [Int] -> ST s Bool
isBm v w ps = VM.read v (toIdx w ps)

nbr :: Int -> Int -> Int -> [Int] -> [[Int]]
nbr w h d [x, y] = l1 ++ l2 ++ l3 ++ l4
  where
    l1 = [[nx, y] | i <- [1..d], let nx = x + i, nx <= w]
    l2 = [[nx, y] | i <- [1..d], let nx = x - i, nx >= 1]
    l3 = [[x, ny] | i <- [1..d], let ny = y + i, ny <= h]
    l4 = [[x, ny] | i <- [1..d], let ny = y - i, ny >= 1]
  
createUF :: Int -> ST s (STVector s Int)
createUF n = VM.replicate n (-1)

rootUF :: STVector s Int -> Int -> ST s Int
rootUF v i = do
  p <- VM.read v i
  if p < 0 then return i
  else do
    r <- rootUF v p
    VM.write v i r
    return r

uniteUF :: STVector s Int -> Int -> Int -> ST s ()
uniteUF v i j = do
  ri <- rootUF v i
  rj <- rootUF v j
  unless (ri == rj) $ do
    ci <- VM.read v ri
    cj <- VM.read v rj
    if ci < cj then do
      VM.write v ri (ci + cj)
      VM.write v rj ri
    else do
      VM.write v ri rj
      VM.write v rj (ci + cj)

sameUF :: STVector s Int -> Int -> Int -> ST s Bool
sameUF v i j = do
  ri <- rootUF v i
  rj <- rootUF v j
  return $ ri == rj

lengthUF :: STVector s Int -> Int -> ST s Int
lengthUF v i = do
  r <- rootUF v i
  l <- VM.read v r
  return $ negate l
  
readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
