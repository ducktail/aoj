import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getl toInt >>= putStrLn

solve :: Int -> String
solve = map f . reverse . unfoldr g 
  where
    f :: Int -> Char
    f 0 = '0'
    f 1 = '+'
    f (-1) = '-'
    g x
      | x == 0 = Nothing
      | otherwise = Just (r-1, q)
      where
        (q,r) = (x+1) `divMod` 3

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
