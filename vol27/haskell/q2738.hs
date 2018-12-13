import Control.Applicative
import Control.Monad
import Data.List
import Data.Bool (bool)

main :: IO ()
main = do
  n <- readLn
  solve <$> replicateM n getLine >>= putStrLn

solve :: [String] -> String
solve = maybe "NO" (bool "NO" "YES" . (== 0)) . foldl' f (Just 0)
  where
    f Nothing _ = Nothing
    f (Just x) "A" = Just (x + 1)
    f (Just x) "Un"
      | x > 0 = Just (x - 1)
      | otherwise = Nothing
