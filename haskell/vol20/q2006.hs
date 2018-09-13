import Control.Applicative
import Control.Monad
import Control.Arrow ((&&&))
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
  n <- readLn
  map solve <$> replicateM n getLine >>= mapM_ putStrLn

solve :: String -> String
solve = reverse . snd . foldl' f (Nothing, []) . map (digitToInt . head &&& length) . group
  where
    ops = ["", ".,!? ", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]
    f (sc, ss) (d, l)
      | d == 0 = case sc of
                  Nothing -> (sc, ss)
                  Just c -> (Nothing, c : ss)
      | otherwise = (Just ((cycle (ops !! d)) !! (l - 1)), ss)
