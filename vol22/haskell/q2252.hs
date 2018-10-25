import Control.Applicative
import Control.Monad
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  s <- getLine
  unless (s == "#") $ do
    print $ solve s
    main

solve :: String -> Int
solve = subtract 1 . length . groupBy f
  where
    lh = S.fromList "qwertasdfgzxcvb"
    f c1 c2 = S.member c1 lh == S.member c2 lh
