import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = solve <$> B.getLine >>= mapM_ putStrLn

solve :: ByteString -> [String]
solve = g . B.foldl' f (0, [], [])
  where f :: (Int, [Int], [(Int, Int)]) -> Char -> (Int, [Int], [(Int, Int)])
        f (i, st1, st2) x | x == '\\' = (i+1, i:st1, st2)
                          | x == '/' = case st1 of
                                        (j:st1') -> let s = i - j
                                                        (st2', st2'') = span ((j<) . fst) st2
                                                    in (i+1, st1', (j, (s + (sum . map snd) st2')):st2'')
                                        [] -> (i+1, st1, st2)
                          | otherwise = (i+1, st1, st2)
        g (_, _, xs) = let ys = map snd xs
                       in [(show . sum) ys, (unwords . map show . ((length ys):) . reverse) ys]
