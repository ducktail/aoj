import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Function (on)

main :: IO ()
main = do
  xs <- getl $ wrds toInt
  unless (xs == [0,0]) $ do
    ys <- replicateM 4 (getl $ wrds toInt)
    putStrLn . solve $ xs : ys
    main
    
solve :: [[Int]] -> String
solve = f . maximumBy (compare `on` snd) . zip ["A","B","C","D","E"] . map sum
  where f (x, y) = x ++ " " ++ show y

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

getc :: ([String] -> a) -> IO a
getc f = f . lines <$> getContents
