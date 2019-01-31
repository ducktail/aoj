import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Vector.Mutable (IOVector, STVector)
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    bp <- replicateM n f
    m <- readLn
    sg <- replicateM m f
    mapM_ putStrLn $ solve n bp sg
    main
  where
    f = map read <$> words <$> getLine
solve :: Int -> [[Int]] -> [[Int]] -> [String]
solve n bp sg = runST $ do
  bv <- VM.replicate n 0 :: ST s (STVector s Int)
  pv <- VM.replicate n (0, 0) :: ST s (STVector s (Int, Int))
  forM_ (zip [0..] bp) $ \(i, [b, x, y]) -> do
    VM.write bv i b
    VM.write pv i (x, y)
  dp <- VM.replicate (n ^ 2) (inf, []) :: ST s (STVector s (Double, [Int]))
  forM_ [0 .. n-1] $ \i -> do
    forM_ [0 .. n-1] $ \j -> do
      p1 <- VM.read pv i
      p2 <- VM.read pv j
      when (under50 p1 p2) $ write dp (i, j) (dist p1 p2, [])
  forM_ [0 .. n-1] $ \k -> do
    forM_ [0 .. n-1] $ \i -> do
      forM_ [0 .. n-1] $ \j -> do
        (d0, _) <- read dp (i, j)
        (d1, l1) <- read dp (i, k)
        (d2, l2) <- read dp (k, j)
        when (d0 > d1 + d2) $ do
          write dp (i, j) (d1 + d2, l1 ++ (k:l2))
  forM sg $ \[s, g] -> do
    si <- findIx bv n s
    gi <- findIx bv n g
    (d, l) <- read dp (si, gi)
    if d == inf
      then return "NA"
      else do
        sl <- forM ([si] ++ l ++ [gi]) $ \i -> do
          j <- VM.read bv i
          return (show j)
        return $ unwords sl
  where
    read v (x, y) = VM.read v (x * n + y)
    write v (x, y) z = VM.write v (x * n + y) z

inf :: Double
inf = 100000.0

eps :: Double
eps = 1e-9

dist :: (Int, Int) -> (Int, Int) -> Double
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

under50 :: (Int, Int) -> (Int, Int) -> Bool
under50 p1 p2 = let d = dist p1 p2
                in abs (d - 50) < eps || d < 50

findIx :: STVector s Int -> Int -> Int -> ST s Int
findIx v n x = foldM (\r i -> do
                         a <- VM.read v i
                         if a == x
                           then return i
                           else return r
                     ) 0 [0 .. n-1]
