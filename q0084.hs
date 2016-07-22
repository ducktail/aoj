import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.List.Split (splitOneOf)

main :: IO ()
main = do
  getLine >>= putStrLn . unwords . solve

solve :: String -> [String]
solve = filter (\s -> length s >= 3 && length s <= 6) . splitOneOf " ,."
