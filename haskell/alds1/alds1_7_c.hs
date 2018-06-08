import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM n (map read <$> words <$> getLine)
  solve n xs

solve :: Int -> [[Int]] -> IO ()
solve n xs = do putStrLn "Preorder"
                printList $ preOrder bt
                putStrLn "Inorder"
                printList $ inOrder bt
                putStrLn "Postorder"
                printList $ postOrder bt
  where ti = mkti n xs
        bt = mkBTree ti (findRoot ti)

mkti :: Int -> [[Int]] -> Vector (Maybe Int, Maybe Int, Maybe Int)
mkti n = foldl' f (V.replicate n (Nothing, Nothing, Nothing))
  where f v [i, l, r] = let x = let (bp, _, _) = v ! i in [(i, (bp, if l == (-1) then Nothing else Just l, if r == (-1) then Nothing else Just r))]
                            y = if l == (-1) then [] else let (_, ll, lr) = v ! l in [(l, (Just i, ll, lr))]
                            z = if r == (-1) then [] else let (_, rl, rr) = v ! r in [(r, (Just i, rl, rr))]
                        in v // (x ++ y ++ z)

findRoot :: Vector (Maybe Int, Maybe Int, Maybe Int) -> Maybe Int
findRoot = V.findIndex f
  where f (Nothing, _, _) = True
        f _ = False

data BTree a = Leaf | Node a (BTree a) (BTree a) deriving Show

mkBTree :: Vector (Maybe Int, Maybe Int, Maybe Int) -> Maybe Int -> BTree Int
mkBTree ti rt = f rt
  where f (Just i) = let (_, l, r) = ti ! i in Node i (f l) (f r)
        f Nothing = Leaf

foldPre :: (a -> b -> b) -> b -> BTree a -> b
foldPre f z Leaf = z
foldPre f z (Node x l r) = let y1 = foldPre f z r
                               y2 = foldPre f y1 l
                           in f x y2

foldIn :: (a -> b -> b) -> b -> BTree a -> b
foldIn f z Leaf = z
foldIn f z (Node x l r) = let y1 = foldIn f z r
                              y2 = f x y1
                          in foldIn f y2 l

foldPost :: (a -> b -> b) -> b -> BTree a -> b
foldPost f z Leaf = z
foldPost f z (Node x l r) = let y1 = f x z
                                y2 = foldPost f y1 r
                            in foldPost f y2 l

preOrder :: BTree a -> [a]
preOrder = foldPre (:) []

inOrder :: BTree a -> [a]
inOrder = foldIn (:) []

postOrder :: BTree a -> [a]
postOrder = foldPost (:) []

printList :: (Show a) => [a] -> IO ()
printList = putStrLn . (' ':) . unwords . map show
