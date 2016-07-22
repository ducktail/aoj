import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input words
  putStrLn $ solve xs
 
solve :: [String] -> String
solve xs = mfw ++ " " ++ maxl
  where maxl = (head . sortBy (\x y -> compare (length y) (length x))) xs
        mfw = (snd . head . sortBy (\x y -> compare (fst y) (fst x)) . map (\x -> (length x, head x)) . group . sort) xs
 
input :: (String -> a) -> IO a
input f = f <$> getLine
