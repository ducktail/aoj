import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- replicateM n f
  m <- readLn
  solve (sort as) <$> replicateM m readLn >>= mapM_ print
  where
    f = map read <$> words <$> getLine

solve :: [[Int]] -> [Int] -> [Int]
solve as = map (maybe (-1) id . f b0 b1 b2)
  where
    (as0, as12) = span (\[_, s] -> s == 0) as
    (as1, as2) = span (\[_, s] -> s == 1) as12
    b0 = if null as0 then Nothing else Just $ (head . last) as0
    b1 = if null as1 then Nothing else Just ((head . head) as1, (head . last) as1)
    b2 = if null as2 then Nothing else Just $ (head . head) as2
    f b0 b1 b2 t = do
      let j0 = do
            x <- b0
            if t <= x then Just 0 else Nothing
      let j1 = do
            (x, y) <- b1
            if x <= t && t <= y then Just 1 else Nothing
      let j2 = do
            x <- b2
            if x <= t then Just 2 else Nothing
      j0 <|> j1 <|> j2
