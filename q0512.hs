import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> getLine >>= putStrLn

solve :: String -> String
solve = map f
  where
    f c = head . drop 23 . dropWhile(/= c) $ ['A'..'Z'] ++ ['A'..'Z']
