import Control.Applicative
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n f >>= print
    main
  where
    f = (\[x, y] -> (x, y)) <$> map read <$> words <$> getLine

solve :: [(Int, Int)] -> Int
solve ps = f 0 ps
  where
    st = S.fromList ps
    f mxa [_] = mxa
    f mxa ((a, b):ps) = let mxa' = foldl' (\ta (c, d) ->
                                          if S.member (b-d+a, c-a+b) st &&
                                             S.member (b-d+c, c-a+d) st
                                          then max ta ((c - a) ^ 2 + (d - b) ^ 2)
                                          else ta
                                        ) mxa ps
                        in f mxa' ps










