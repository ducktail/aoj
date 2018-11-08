import Control.Applicative
import Data.Bool (bool)

main :: IO ()
main = solve <$> getLine >>= putStrLn

solve :: String -> String
solve = bool "Rabbit" "Cat" . catcheck

catcheck :: String -> Bool
catcheck "" = True
catcheck ('m' : xs) = f 1 [0] [] xs
  where
    f _ [] [] [] = True
    f _ [] [] _ = False
    f i ms es ('m':xs) = f (i+1) (i:ms) es xs
    f i ms es ('e':xs) = f (i+1) ms (i:es) xs
    f i (m:ms) (e:es) ('w':xs)
      | m < e && e < i = f (i+1) ms es xs
      | otherwise = False
    f _ _ _ _ = False                
catcheck _ = False
