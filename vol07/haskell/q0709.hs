import Control.Applicative
import Data.List
import Data.Bool (bool)
import qualified Data.Set as S

main :: IO ()
main = getLine >> solve <$> getLine >>= putStrLn

solve :: String -> String
-- solve = bool "No" "Yes" . (>= 3) . length . group . sort
solve = bool "No" "Yes" . (>= 3) . S.size . foldl (flip S.insert) S.empty










