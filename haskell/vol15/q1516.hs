import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = solve <$> f >>= mapM_ B.putStrLn
  where
    f = B.lines <$> B.getContents

solve :: [ByteString] -> [ByteString]
solve = filter f
  where
    f bs = all isNeighbor $ B.zip bs (B.tail bs)

isNeighbor :: (Char, Char) -> Bool
isNeighbor ('A', 'B') = True
isNeighbor ('A', 'D') = True
isNeighbor ('B', 'A') = True
isNeighbor ('B', 'E') = True
isNeighbor ('B', 'C') = True
isNeighbor ('C', 'B') = True
isNeighbor ('C', 'F') = True
isNeighbor ('D', 'A') = True
isNeighbor ('D', 'E') = True
isNeighbor ('D', 'G') = True
isNeighbor ('E', 'B') = True
isNeighbor ('E', 'D') = True
isNeighbor ('E', 'F') = True
isNeighbor ('E', 'H') = True
isNeighbor ('F', 'C') = True
isNeighbor ('F', 'E') = True
isNeighbor ('F', 'I') = True
isNeighbor ('G', 'D') = True
isNeighbor ('G', 'H') = True
isNeighbor ('H', 'G') = True
isNeighbor ('H', 'E') = True
isNeighbor ('H', 'I') = True
isNeighbor ('I', 'F') = True
isNeighbor ('I', 'H') = True
isNeighbor (_, '\r') = True -- <- for windows
isNeighbor _ = False
