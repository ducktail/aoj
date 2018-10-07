import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split (splitOn)
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  n <- readLn
  [h, w] <- f
  solve h w <$> replicateM h getLine <*> (getLine >> getLine) >>= mapM_ putStrLn
  replicateM_ (n - 1) $ do
    putStrLn ""
    [h, w] <- f
    solve h w <$> replicateM h getLine <*> (getLine >> getLine) >>= mapM_ putStrLn
  where
    f = map read <$> words <$> getLine

solve :: Int -> Int -> [String] -> String -> [String]
solve h w ss cs = let iv = V.fromList $ replicate (w + 1) ' ' ++ intercalate " " ss ++ replicate (w + 1) ' '
                      Just ii = find (\i -> let c = iv ! i in c == '^' || c == 'v' || c == '<' || c == '>') [0..]
                      is = (fromIdx ii, iv)
                  in evalState (f cs) is
  where
    toIdx (i, j) = (i + 1) * (w + 1) + j
    fromIdx ix = let (q, r) = divMod ix (w + 1) in (q - 1, r)

    f :: String -> State ((Int, Int), Vector Char) [String]
    f [] = do
      (_, v) <- get
      return $ splitOn " " $ take (h * (w + 1) - 1) $ drop (w + 1) $ V.toList v

    f (c:cs) = do
      ((i, j), v) <- get
      case c of
       'U' -> if v ! (toIdx (i - 1, j)) == '.'
              then put ((i - 1, j), v // [(toIdx (i - 1, j), '^'),(toIdx (i, j), '.')])
              else put ((i, j), v // [(toIdx (i, j), '^')])
       'D' -> if v ! (toIdx (i + 1, j)) == '.'
              then put ((i + 1, j), v // [(toIdx (i + 1, j), 'v'),(toIdx (i, j), '.')])
              else put ((i, j), v // [(toIdx (i, j), 'v')])
       'L' -> if v ! (toIdx (i, j - 1)) == '.'
              then put ((i, j - 1), v // [(toIdx (i, j - 1), '<'),(toIdx (i, j), '.')])
              else put ((i, j), v // [(toIdx (i, j), '<')])
       'R' -> if v ! (toIdx (i, j + 1)) == '.'
              then put ((i, j + 1), v // [(toIdx (i, j + 1), '>'),(toIdx (i, j), '.')])
              else put ((i, j), v // [(toIdx (i, j), '>')])
       'S' -> let ixs = case v ! (toIdx (i, j)) of
                    '^' -> zip [i-1, i-2 .. 0] (repeat j)
                    'v' -> zip [i+1 .. h-1] (repeat j)
                    '<' -> zip (repeat i) [j-1, j-2 .. 0]
                    '>' -> zip (repeat i) [j+1 .. w-1]
              in case find (\(x,y) -> let c = v ! (toIdx (x, y)) in c == '*' || c == '#') ixs of
                  Nothing -> put ((i, j), v)
                  Just (x, y) -> if v ! (toIdx (x, y)) == '*'
                                 then put ((i, j), v // [(toIdx (x, y), '.')])
                                 else put ((i, j), v)
      f cs
