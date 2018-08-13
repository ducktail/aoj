import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM)
import Data.Char (isDigit, digitToInt, intToDigit)

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n $ do
    o <- getLine
    m <- getLine
    return [o, m]
  mapM_ (putStrLn . solve) ss

solve :: [String] -> String
solve [o, m] = foldr f m o
  where f 'J' s = (last s) : (init s)
        f 'C' s = (tail s) ++ [head s]
        f 'E' s = let (as, bs) = splitAt (length s `div` 2) s
                  in if even (length s) then bs ++ as
                     else (tail bs) ++ (head bs : as)
        f 'A' s = reverse s
        f 'P' s = map (\c -> if isDigit c then (intToDigit . (flip mod 10) . (subtract 1) . digitToInt) c else c) s
        f 'M' s = map (\c -> if isDigit c then (intToDigit . (flip mod 10) . (+1) . digitToInt) c else c) s
