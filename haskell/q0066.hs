import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = getc solve >>= mapM_ putStrLn

solve :: String -> String
solve ss
  | sorou 'o' = "o"
  | sorou 'x' = "x"
  | otherwise = "d"
  where
    tate c i = ss !! i == c && ss !! (i + 3) == c && ss !! (i + 6) == c
    yoko c i = ss !! i == c && ss !! (i + 1) == c && ss !! (i + 2) == c
    naname c = (ss !! 0 == c && ss !! 4 == c && ss !! 8 == c) || (ss !! 2 == c && ss !! 4 == c && ss !! 6 == c)
    sorou c = any (tate c) [0..2] || any (yoko c) [0,3,6] || naname c
getc :: (String -> a) -> IO [a]
getc f = map f . lines <$> getContents
