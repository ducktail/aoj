import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getl (wrds toInt) >>= mapM_ putStrLn

solve :: [Int] -> [String]
solve [j,y] = sort . map reverse . game $ (j,y)

game :: (Int,Int) -> [String]
game (6,4) = map ('A':) (game (5,4))
game (4,6) = map ('B':) (game (4,5))
game (5,5) = map ('A':) (game (4,5)) ++ map ('B':) (game (5,4))
game (5,y) = map ('A':) (game (4,y))
game (j,5) = map ('B':) (game (j,4))
game (j,y) = (nub . permutations) (replicate j 'A' ++ replicate y 'B')

toInt :: String -> Int
toInt = read

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
