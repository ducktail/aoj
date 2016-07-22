import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List
import Data.Char (digitToInt)

main :: IO ()
main = do
  [x,y] <- getl words
  unless (x == "0" && y == "0") $ do
    putStrLn . unwords . map show $ solve x y
    main
    
solve :: String -> String -> [Int]
solve x y = [h, length (intersect qs as) - h]
  where
    f = map digitToInt
    hit xs ys = sum $ zipWith (\a b -> if a == b then 1 else 0) xs ys
    qs = f x
    as = f y
    h = hit qs as
      
getl :: (String -> a) -> IO a
getl f = f <$> getLine
