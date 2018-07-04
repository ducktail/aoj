import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  [n, m] <- map read <$> words <$> getLine
  solve n <$> replicateM n getLine >>= print

solve :: Int -> [String] -> Int
solve n ss = minimum ps
  where v = V.unfoldr (\(ss, (w,b,r)) ->
                        case ss of
                         (s:ss) -> let (w',b',r') = cnt s in Just ((w+w',b+b',r+r'), (ss, (w+w',b+b',r+r')))
                         _ -> Nothing
                      ) (ss,(0,0,0))
        ps = do
          wi <- [0 .. n-3]
          bi <- [wi+1 .. n-2]
          let (w1, b1, r1) = v ! wi
          let (w2, b2, r2) = v ! bi
          let (w3, b3, r3) = v ! (n-1)
          return $ b1 + r1 + w2 - w1 + r2 - r1 + w3 - w2 + b3 - b2
          
cnt :: String -> (Int, Int, Int)
cnt s = foldl' (\(w, b, r) c -> case c of
                       'W' -> (w+1, b, r)
                       'B' -> (w, b+1, r)
                       _ -> (w, b, r+1)
               ) (0, 0, 0) s
