import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  forM_ [1..9] $ \i -> do
    forM_ [1..9] $ \j -> do
      putStrLn $ qq i j
 
qq :: Int -> Int -> String
qq x y = show x ++ "x" ++ show y ++ "=" ++ show (x * y)

-- main = mapM_ putStrLn qq
-- 
-- qq :: [String]
-- qq = map (\(a, b) -> show a ++ "x" ++ show b ++ "=" ++ show (a * b)) idx
--   where
--     idx = [(i,j) | i <- [1..9], j <- [1..9]]
