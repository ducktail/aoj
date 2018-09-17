import Control.Applicative ((<$>))
import Control.Monad (unless)
import Data.List (intercalate)
import Data.List.Split (splitOn, split, startsWithOneOf)
import Data.Char (toUpper, toLower)

main :: IO ()
main = do
  [nm, tp] <- words <$> getLine
  unless (tp == "X") $ do
    putStrLn $ solve nm tp
    main

solve :: String -> String -> String
solve nm tp = f tp $ toWords nm
  where f t ss | t == "D" = intercalate "_" ss
               | t == "U" = concat . map capitalize $ ss
               | otherwise = concat . g $ ss
        g (s:ss) = s : map capitalize ss
          
toWords :: String -> [String]
toWords s | '_' `elem` s = splitOn "_" s
          | otherwise = map (map toLower) $ split (startsWithOneOf ['A'..'Z']) s

capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = toUpper s : ss
