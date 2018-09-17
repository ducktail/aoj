import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Bool (bool)

main :: IO ()
main = do
  B.getLine
  B.lines <$> B.getContents >>= solve

solve :: [ByteString] -> IO ()
solve bs = foldM f Leaf bs >> return ()
  where f :: BSTree Int -> ByteString -> IO (BSTree Int)
        f tr bs = case B.words bs of
                   [com, bn] | com == sfind -> do
                     putStrLn $ bool "no" "yes" $ bstFind (readi B.readInt bn) tr
                     return tr
                   [_, bn] -> return (bstInsert (readi B.readInt bn) tr)
                   _ -> do
                     printList $ inOrder tr
                     printList $ preOrder tr
                     return tr
        sfind = B.pack "find"

data BSTree a = Leaf | Node a (BSTree a) (BSTree a) deriving Show

bstInsert :: (Ord a) => a -> BSTree a -> BSTree a
bstInsert x (Node y lt rt) | x > y = Node y lt (bstInsert x rt)
                           | x < y = Node y (bstInsert x lt) rt
                           | otherwise = Node y lt rt
bstInsert x Leaf = Node x Leaf Leaf

bstFind :: (Ord a) => a -> BSTree a -> Bool
bstFind _ Leaf = False
bstFind x (Node y lt rt) | x == y = True
                         | x > y = bstFind x rt
                         | otherwise = bstFind x lt

foldPre :: (a -> b -> b) -> b -> BSTree a -> b
foldPre f z Leaf = z
foldPre f z (Node x l r) = f x y2
  where y1 = foldPre f z r
        y2 = foldPre f y1 l

foldIn :: (a -> b -> b) -> b -> BSTree a -> b
foldIn f z Leaf = z
foldIn f z (Node x l r) = foldIn f y2 l
  where y1 = foldIn f z r
        y2 = f x y1

preOrder :: BSTree a -> [a]
preOrder = foldPre (:) []

inOrder :: BSTree a -> [a]
inOrder = foldIn (:) []

printList :: (Show a) => [a] -> IO ()
printList = putStrLn . (' ':) . unwords . map show

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
