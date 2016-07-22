import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- replicateM 8 getLine
  solve xs

solve :: [String] -> IO ()
solve xs = do
  let r90 = rot90 xs
  putStrLn "90"
  mapM_ putStrLn r90
  let r180 = rot90 r90
  putStrLn "180"
  mapM_ putStrLn r180
  let r270 = rot90 r180
  putStrLn "270"
  mapM_ putStrLn r270

rot90 :: [String] -> [String]
rot90 = map reverse . transpose
