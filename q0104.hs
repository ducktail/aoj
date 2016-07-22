import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  [h,w] <- getl $ split toInt
  when (h /= 0 || w /= 0) $ do
    xs <- replicateM h getLine
    putStrLn . solve $ xs
    main

solve :: [String] -> String
solve xs = case p of
            Nothing   ->  "LOOP"
            Just (x,y) ->  show x ++ " " ++ show y
  where
    p = move xs [] (0,0)
    
move :: [String] -> [(Int,Int)] -> (Int,Int) -> Maybe (Int, Int)
move mt hs (x,y)
  | (x,y) `elem` hs = Nothing
  | c == '.' = Just (x,y)
  | c == '<' = move mt ((x,y):hs) (x-1,y)
  | c == '>' = move mt ((x,y):hs) (x+1,y)
  | c == '^' = move mt ((x,y):hs) (x,y-1)
  | c == 'v' = move mt ((x,y):hs) (x,y+1)
  | otherwise = Nothing
  where
    c = mt !! y !! x
    
toInt :: String -> Int
toInt s = read s

split :: (String -> a) -> String -> [a]
split f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
