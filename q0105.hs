import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import qualified Data.Map as M

main :: IO ()
main = mapM_ printIndex . solve =<< getc words

solve :: [[String]] -> [(String,[String])]
solve xs = M.assocs $ foldl f M.empty xs
  where
    f m [s,n] = M.insertWith (++) s [n] m

printIndex :: (String,[String]) -> IO ()
printIndex (s,ss) = do
  putStrLn s
  putStrLn . unwords . sortBy (\x y -> (toInt x) `compare` (toInt y)) $ ss
  
toInt :: String -> Int
toInt s = read s

getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
