import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Control.Monad.State
import Data.Array.Unboxed

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve n <$> getLine >>= putStrLn
    main

solve :: Int -> String -> String
solve n xs = f $ execState (game xs (cycle [1..n])) (listArray (1,n) (repeat 0), 0)
  where
    f (a,f) = (unwords . map show . sort . elems) a ++ " " ++ show f

game :: String -> [Int] -> State (UArray Int Int, Int) ()
game [] _ = return ()
game (b:bs) (i:is) = do
  (a,f) <- get
  case b of
   'M' -> put (accum (+) a [(i,1)], f)
   'L' -> put (accum (+) a [(i,1 + f)], 0)
   'S' -> put (a // [(i,0)],f + a ! i + 1)
  game bs is

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
