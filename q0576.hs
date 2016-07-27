import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  solve <$> replicateM 5 (getl toInt) >>= print

solve :: [Int] -> Int
solve [l,a,b,c,d] = l - max (f a c) (f b d)
  where
    f x y = let (q,r) = x `divMod` y in if r == 0 then q else q + 1

toInt :: String -> Int
toInt = read

getl :: (String -> a) -> IO a
getl f = f <$> getLine
