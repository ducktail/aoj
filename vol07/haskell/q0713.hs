import Control.Applicative
import Data.Maybe(mapMaybe)

main :: IO ()
main = getLine >> solve <$> getLine >>= mapM_ putStrLn

solve :: String -> [String]
solve = mapMaybe f . (tail >>= zip)
  where
    f (c1, c0) = if c1 == 'J' then Just [c0] else Nothing
