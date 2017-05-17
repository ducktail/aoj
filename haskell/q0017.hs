import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (ord)

main = do
  ss <- lines <$> getContents
  mapM_ putStrLn $ solve ss
 
solve :: [String] -> [String]
solve = map cipher
 
cipher :: String -> String
cipher s | (or . map (flip isInfixOf ns)) ["the", "this", "that"] = ns
        | otherwise = cipher ns
  where ns = map (rot 1) s
 
rot :: Int -> Char -> Char
rot n c | c `elem` tbl = tbl !! (ord c - ord 'a' + n)
        | otherwise = c
  where tbl = ['a'..'z'] ++ ['a'..'z']
