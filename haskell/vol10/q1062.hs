import Control.Applicative ((<$>))
import Control.Monad (replicateM, unless)
import Data.List.Split (splitOneOf)

main :: IO ()
main = do
  n <- readLn
  unless (n == 0) $ do
    solve <$> replicateM n (map read <$> splitOneOf ": " <$> getLine) >>= mapM_ putStrLn
    main

solve :: [[Int]] -> [String]
solve xs = ["lunch " ++ ratio ls, "dinner " ++ ratio ds, "midnight " ++ ratio ms]
  where ms = filter (\(h:_) -> h >= 21 || h < 2) xs
        ds = filter (\(h:_) -> h >= 18 && h < 21) xs
        ls = filter (\(h:_) -> h >= 11 && h < 15) xs
ratio :: [[Int]] -> String
ratio xs | null xs = "no guest"
         | otherwise = show $ length (filter f xs) * 100 `div` length xs
  where f [h,sm,em] | sm > em = 60 + em - sm <= 8
                    | otherwise = em - sm <= 8
