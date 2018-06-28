import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Data.Char (isSpace)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  [n, m] <- readil B.readInt <$> B.getLine
  solve n m

solve :: Int -> Int -> IO ()
solve n m = do
  gd <- VM.replicate n []
  forM_ [1..m] $ \_ -> do
    [u, v] <- readil B.readInt <$> B.getLine
    ul <- VM.read gd u
    VM.write gd u (v:ul)
    vl <- VM.read gd v
    VM.write gd v (u:vl)
  idv <- VM.replicate n 0
  forM_ [0..n-1] $ \i -> do
    ii <- VM.read idv i
    when (ii == 0) $ do
      VM.write idv i (i+1)
      dfs gd idv (i+1) [i]
  q <- readi B.readInt <$> B.getLine
  forM_ [1..q] $ \_ -> do
    [s, t] <- readil B.readInt <$> B.getLine
    si <- VM.read idv s
    ti <- VM.read idv t
    putStrLn $ if si == ti then "yes" else "no"
  where dfs :: IOVector [Int] -> IOVector Int -> Int -> [Int] -> IO ()
        dfs gd idv idt [] = return ()
        dfs gd idv idt (u:st) = do
          nbl <- VM.read gd u
          mv <- findM (\j -> do
                          jid <- VM.read idv j
                          return (jid == 0)) nbl
          case mv of
           (Just v) -> do
             VM.write idv v idt
             dfs gd idv idt (v:u:st)
           Nothing -> dfs gd idv idt st

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b then return $ Just x
    else findM p xs
