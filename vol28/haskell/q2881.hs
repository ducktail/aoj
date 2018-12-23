import Control.Applicative
import Control.Monad

main :: IO ()
main = do
  s <- getLine
  unless (s == "#") $ do
    putStrLn $ solve $ words s
    main

solve :: [String] -> String
solve [g, y, m, d]
  | iy < 31 || iy == 31 && im < 5 = unwords [g, y, m, d]
  | otherwise = unwords ["?", show (iy - 30), m, d]
  where
    iy = read y
    im = read m
