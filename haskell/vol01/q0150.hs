import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless, when, forM_)
import Data.List (find)

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve tbl n
    main
  where sz = 10000
        tbl = V.create $ do
          v <- VM.replicate (sz+1) True
          VM.write v 1 False
          forM_ [2 .. sz] $ \i -> do
            p <- VM.read v i
            when p $ do
              forM_ [i*i, i*(i+1) .. sz] $ \j -> do
                VM.write v j False
          return v

solve :: Vector Bool -> Int -> IO ()
solve tbl n = putStrLn . maybe "" (\x -> show (x-2) ++ " " ++ show x) . find f $ [n, n-1 .. 5]
  where f x = tbl ! x && tbl ! (x-2)
