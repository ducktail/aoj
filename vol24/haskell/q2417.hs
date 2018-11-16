import Control.Applicative
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

main :: IO ()
main = solve <$> getLine >>= putStrLn

solve :: String -> String
solve = f
  where
    cm = M.fromList [('2', 'k'), ('3', 's'), ('4', 't'), ('5', 'n'),
                     ('6', 'h'), ('7', 'm'), ('8', 'y'), ('9', 'r'), ('0', 'w')]
    vm = M.fromList [('T', 'a'), ('L', 'i'), ('U', 'u'), ('R', 'e'), ('D', 'o')]
    f [] = []
    f (x:y:ys)
      | x == '0' && y == 'U' = 'n' : 'n' : f ys
      | x == '1' = vm ! y : f ys
      | otherwise = cm ! x : vm ! y : f ys
