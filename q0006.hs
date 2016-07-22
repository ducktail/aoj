import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  ss <- getLine
  putStrLn $ reverse' ss
 
reverse' :: String -> String
reverse' = foldl (flip (:)) []
