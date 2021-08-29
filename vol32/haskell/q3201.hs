import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List
import Data.Char (isSpace)
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [n, m] <- f
  unless (n == 0 && m == 0) $ do
    as <- f
    bs <- f
    solve as bs
    main
  where
    f = readil B.readInt <$> B.getLine

solve :: [Int] -> [Int] -> IO ()
solve as bs = do
  vc <- VM.replicate 10 0 :: IO (IOVector Int)
  forM_ [a*b|a <- as, b <- bs] $ \x -> do
    forM_ (toDgs x) $ \y -> do      
      incCt vc y
  ls <- forM [0..9] $ \i -> VM.read vc i
  putStrLn . unwords . map show $ ls

incCt :: IOVector Int -> Int -> IO ()
incCt v i = do
  x <- VM.read v i
  VM.write v i (x + 1)

toDgs x = unfoldr f x
  where
    f x = case x `divMod` 10 of
      (0, 0) -> Nothing
      (q, r) -> Just (r, q)
readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr $ f . B.dropWhile isSpace
