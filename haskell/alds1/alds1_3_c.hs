import Control.Applicative ((<$>))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (unfoldr)
import Data.Foldable (foldl')
import Data.Char (isSpace)
import Data.Sequence ((<|), (><), ViewL(..), ViewR(..), Seq)
import qualified Data.Sequence as Sq

main :: IO ()
main = B.getLine >> solve <$> map B.words <$> B.lines <$> B.getContents >>= B.putStrLn

solve :: [[ByteString]] -> ByteString
solve = B.unwords . foldr (:) [] . foldl' f Sq.empty
  where f :: Seq ByteString -> [ByteString] -> Seq ByteString
        f sq [com, x] | com == ins = x <| sq
                      | otherwise = let (Just i) = Sq.elemIndexL x sq
                                        (asq, bsq) = Sq.splitAt i sq
                                    in asq >< (Sq.drop 1 bsq)
                      -- | otherwise = let (Just i) = Sq.elemIndexL x sq
                      --               in Sq.deleteAt i sq
          where ins = B.pack "insert"
        f sq [com] | com == delf = case Sq.viewl sq of
                                    _ :< nsq -> nsq
                   | otherwise = case Sq.viewr sq of
                                  nsq :> _ -> nsq
          where delf = B.pack "deleteFirst"

readil :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> [a]
readil f = unfoldr g
  where
    g s = do
      (n, s') <- f s
      return (n, B.dropWhile isSpace s')
