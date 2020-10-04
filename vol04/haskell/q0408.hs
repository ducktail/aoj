import Control.Applicative
import Control.Monad

main :: IO ()
main = getLine >> solve <$> f >>= putStrLn
  where
    f = map read <$> words <$> getLine

solve :: [Int] -> String
solve = either show (const "OK") . foldM f [] . zip [1..]
  where
    f [] (i, c)
      | c > 0 = return [c]
      | otherwise = Left i
    f st @ (x:xs) (i, c)
      | c > 0 && c `notElem` st = return (c:st)
      | c < 0 && negate c == x = return xs
      | otherwise = Left i
