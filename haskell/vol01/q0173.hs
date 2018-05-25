import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = replicateM_ 9 solve

solve :: IO ()
solve = do
  xs <- words <$> getLine
  let m  = toInt (xs !! 1)
      af = toInt (xs !! 2)
  putStrLn $ (xs !! 0) ++ " " ++ show (m + af) ++ " " ++ show (m * 200 + af * 300)

toInt :: String -> Int
toInt s = read s
