import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (splitOn)
import Data.Char (digitToInt, intToDigit)

main :: IO ()
main = do
  s <- getLine
  unless (s == "#") $ do
    [a, b, c, d] <- f :: IO [Int]
    putStrLn . intercalate "/" . map enc . solve (a, b) (c, d) . map dec $ splitOn "/" s
    main
  where
    f = map read <$> words <$> getLine

solve :: (Int, Int) -> (Int, Int) -> [String] -> [String]
solve (a, b) (c, d) = exch (c, d) 'b' . exch (a, b) '.'

dec :: String -> String
dec = concatMap f
  where
    f 'b' = "b"
    f x = replicate (digitToInt x) '.'

enc :: String -> String
enc = concatMap f . group
  where
    f xs
      | head xs == 'b' = xs
      | otherwise = [intToDigit $ length xs]

exch :: (Int, Int) -> Char -> [String] -> [String]
exch (i, j) ch xss = let (as, b:bs) = splitAt (i - 1) xss
                         (cs, d:ds) = splitAt (j - 1) b
                     in as ++ (cs ++ (ch : ds)) : bs
