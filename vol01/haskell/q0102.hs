import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- getl toInt
  when (n /= 0) $ do
    xs <- rgetl n $ split toInt
    mapM_ (putStrLn . concat . map format) $ solve xs
    main
    
solve :: [[Int]] -> [[Int]]
solve = map (\ss -> ss ++ [sum ss]) . transpose . map (\ss -> ss ++ [sum ss]) . transpose
    
format :: Int -> String
format x
  | x < 10 = "    " ++ show x
  | x < 100 = "   " ++ show x
  | x < 1000 = "  " ++ show x
  | x < 10000 = " " ++ show x
  | otherwise = show x

toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine

rgetl :: Int -> (String -> a) -> IO [a]
rgetl n f = map f <$> replicateM n getLine
