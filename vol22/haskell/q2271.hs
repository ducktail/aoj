import Control.Applicative
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

main :: IO ()
main = solve <$> getLine >>= print

solve :: String -> Int
solve s = let imp = M.fromList [('K',0),('U',0),('P',0),('C',0)]
              mp = foldl' (\m c -> M.insertWith (+) c 1 m) imp s
          in minimum $ map (mp !) "KUPC"
