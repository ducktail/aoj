import Control.Applicative ((<$>))
import Control.Monad (replicateM, forM_, when)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  [m, n] <- map read <$> words <$> getLine
  k <- readLn
  solve m n k

solve :: Int -> Int -> Int -> IO ()
solve m n k = do
  dpj <- VM.replicate ((m+1) * (n+1)) 0 :: IO (IOVector Int)
  dpo <- VM.replicate ((m+1) * (n+1)) 0 :: IO (IOVector Int)
  dpi <- VM.replicate ((m+1) * (n+1)) 0 :: IO (IOVector Int)
  forM_ [1..m] $ \i -> do
    s <- getLine
    forM_ (zip [1..n] s) $ \(j, c) -> do
      lj <- VM.read dpj ((n+1)*i+j-1)
      uj <- VM.read dpj ((n+1)*i+j-n-1)
      ulj <- VM.read dpj ((n+1)*i+j-n-2)
      VM.write dpj ((n+1)*i+j) (uj + lj - ulj)
      lo <- VM.read dpo ((n+1)*i+j-1)
      uo <- VM.read dpo ((n+1)*i+j-n-1)
      ulo <- VM.read dpo ((n+1)*i+j-n-2)
      VM.write dpo ((n+1)*i+j) (uo + lo - ulo)
      li <- VM.read dpi ((n+1)*i+j-1)
      ui <- VM.read dpi ((n+1)*i+j-n-1)
      uli <- VM.read dpi ((n+1)*i+j-n-2)
      VM.write dpi ((n+1)*i+j) (ui + li - uli)
      when (c == 'J') $ do
        modify dpj (+1) ((n+1)*i+j) 
      when (c == 'O') $ do
        modify dpo (+1) ((n+1)*i+j)
      when (c == 'I') $ do
        modify dpi (+1) ((n+1)*i+j)
  forM_ [1..k] $ \_ -> do
    [a, b, c, d] <- map read <$> words <$> getLine
    lj <- VM.read dpj (c*(n+1)+(b-1))
    uj <- VM.read dpj ((a-1)*(n+1)+d)
    ulj <- VM.read dpj ((a-1)*(n+1)+(b-1))
    ctj <- VM.read dpj (c*(n+1)+d)
    lo <- VM.read dpo (c*(n+1)+(b-1))
    uo <- VM.read dpo ((a-1)*(n+1)+d)
    ulo <- VM.read dpo ((a-1)*(n+1)+(b-1))
    cto <- VM.read dpo (c*(n+1)+d)
    li <- VM.read dpi (c*(n+1)+(b-1))
    ui <- VM.read dpi ((a-1)*(n+1)+d)
    uli <- VM.read dpi ((a-1)*(n+1)+(b-1))
    cti <- VM.read dpi (c*(n+1)+d)
    putStrLn . unwords . map show $ [ctj - lj - uj + ulj, cto - lo - uo + ulo, cti - li - ui + uli]

modify :: IOVector Int -> (Int -> Int) -> Int -> IO ()
modify v f i = do
  x <- VM.read v i
  VM.write v i (f x)
