import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  unless (n == 0) $ do
    solve <$> replicateM n getLine >>= print
    main

solve :: [String] -> Int
solve = sum . map head . scanl f [1,2,3,4,5,6]
  where
    f [d1,d2,d3,d4,d5,d6] s
      | s == "North" = [d2,d6,d3,d4,d1,d5]
      | s == "East" = [d4,d2,d1,d6,d5,d3]
      | s == "West" = [d3,d2,d6,d1,d5,d4]
      | s == "South" = [d5,d1,d3,d4,d6,d2]
      | s == "Right" = [d1,d3,d5,d2,d4,d6]
      | otherwise = [d1,d4,d2,d5,d3,d6]

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
