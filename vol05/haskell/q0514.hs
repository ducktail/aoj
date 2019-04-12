import Control.Applicative
import Control.Monad
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

main :: IO ()
main = do
  [a, b, c] <- f
  unless (a == 0 && b == 0 && c == 0) $ do
    n <- readLn
    solve (a+b+c) <$> replicateM n f >>= mapM_ print
    main
  where
    f = map read <$> words <$> getLine

solve :: Int -> [[Int]] -> [Int]
solve abc xss = map f [1 .. abc]
  where
    rms = foldl' (\s [i, j, k, r] -> if r == 0
                                     then s
                                     else IS.insert i (IS.insert j (IS.insert k s))) IS.empty xss
    bms = foldl' (\s [i, j, k, r] -> case (IS.member i rms, IS.member j rms, IS.member k rms) of
                                      (True, True, False) -> IS.insert k s
                                      (True, False, True) -> IS.insert j s
                                      (False, True, True) -> IS.insert i s
                                      _ -> s
                   ) IS.empty xss
    f x = if IS.member x bms
          then 0
          else if IS.member x rms
               then 1
               else 2
