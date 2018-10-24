import Control.Applicative
import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, q] <- f
  unless (n == 0 && q == 0) $ do
    solve <$> (sort <$> replicateM n g) <*> replicateM q readLn >>= mapM_ putStrLn
    main
  where
    f = map read <$> words <$> getLine
    g = (\[en, eby, wy] -> (read wy, read wy - read eby + 1, en)) <$> words <$> getLine

solve :: [(Int, Int, String)] -> [Int] -> [String]
solve yt = map f
  where
    f q = let (_, ys) = break (\(y, _, _) -> y >= q) yt
          in case ys of
              (lwy, fwy, en):_ | fwy <= q -> unwords [en, show (q - fwy + 1)]
              _ -> "Unknown"
