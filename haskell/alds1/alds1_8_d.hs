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
  where f :: Treap Int -> ByteString -> IO (Treap Int)
        f tr bs = case B.words bs of
                   [_, bk, bp] -> do
                     return (trpInsert (readi B.readInt bk) (readi B.readInt bp) tr)
                   [com, bk] | com == sfind -> do
                                 putStrLn $ bool "no" "yes" $ trpFind (readi B.readInt bk) tr
                                 return tr
                             | otherwise -> do
                                 return (trpDelete (readi B.readInt bk) tr)
                   _ -> do
                     printList $ inOrder tr
                     printList $ preOrder tr
                     return tr
        sfind = B.pack "find"
        
data Treap a = Leaf | Node Int a (Treap a) (Treap a) deriving Show

trpRightRotate :: Treap a -> Treap a
trpRightRotate Leaf = Leaf
trpRightRotate (Node yp yv Leaf yrt) = Node yp yv Leaf yrt
trpRightRotate (Node yp yv (Node xp xv xlt xrt) yrt) = Node xp xv xlt (Node yp yv xrt yrt)

trpLeftRotate :: Treap a -> Treap a
trpLeftRotate Leaf = Leaf
trpLeftRotate (Node xp xv xlt Leaf) = Node xp xv xlt Leaf
trpLeftRotate (Node xp xv xlt (Node yp yv ylt yrt)) = Node yp yv (Node xp xv xlt ylt) yrt

trpNormalize :: Treap a -> Treap a
trpNormalize Leaf = Leaf
trpNormalize (Node xp xv Leaf Leaf) = (Node xp xv Leaf Leaf)
trpNormalize xtr@(Node yp _ (Node xp _ _ _) Leaf) | xp > yp =  trpRightRotate xtr
                                                  | otherwise = xtr
trpNormalize ytr@(Node xp _ Leaf (Node yp _ _ _)) | yp > xp = trpLeftRotate ytr
                                                  | otherwise = ytr
trpNormalize xtr@(Node xp _ (Node yp _ _ _) (Node zp _ _ _)) | yp > xp = trpRightRotate xtr
                                                             | zp > xp = trpLeftRotate xtr
                                                             | otherwise = xtr

trpInsert :: (Ord a) => a -> Int -> Treap a -> Treap a
trpInsert xv xp (Node yp yv lt rt) | xv > yv = trpNormalize (Node yp yv lt (trpInsert xv xp rt))
                                   | xv < yv = trpNormalize (Node yp yv (trpInsert xv xp lt) rt)
                                   | otherwise = Node yp yv lt rt
trpInsert xv xp Leaf = Node xp xv Leaf Leaf

trpFind :: (Ord a) => a -> Treap a -> Bool
trpFind _ Leaf = False
trpFind x (Node _ y lt rt) | x == y = True
                           | x > y = trpFind x rt
                           | otherwise = trpFind x lt

trpDelete :: (Ord a) => a -> Treap a -> Treap a
trpDelete x Leaf = Leaf
trpDelete x (Node yp yv lt rt) | x < yv = Node yp yv (trpDelete x lt) rt
                               | x > yv = Node yp yv lt (trpDelete x rt)
                               | otherwise = case (lt, rt) of
                                              (Leaf, Leaf) -> Leaf
                                              (Leaf, rt) -> let (Node tp tv tlt trt) = trpLeftRotate (Node yp yv lt rt)
                                                            in Node tp tv (trpDelete x tlt) trt
                                              (lt, Leaf) -> let (Node tp tv tlt trt) = trpRightRotate (Node yp yv lt rt)
                                                            in Node tp tv tlt (trpDelete x trt)
                                              (Node lp _ _ _, Node rp _ _ _) | lp > rp -> let (Node tp tv tlt trt) = trpRightRotate (Node yp yv lt rt)
                                                                                          in Node tp tv tlt (trpDelete x trt)
                                                                             | otherwise -> let (Node tp tv tlt trt) = trpLeftRotate (Node yp yv lt rt)
                                                                                            in Node tp tv (trpDelete x tlt) trt

foldPre :: (a -> b -> b) -> b -> Treap a -> b
foldPre f z Leaf = z
foldPre f z (Node _ x l r) = f x y2
  where y1 = foldPre f z r
        y2 = foldPre f y1 l

foldIn :: (a -> b -> b) -> b -> Treap a -> b
foldIn f z Leaf = z
foldIn f z (Node _ x l r) = foldIn f y2 l
  where y1 = foldIn f z r
        y2 = f x y1

preOrder :: Treap a -> [a]
preOrder = foldPre (:) []

inOrder :: Treap a -> [a]
inOrder = foldIn (:) []

printList :: (Show a) => [a] -> IO ()
printList = putStrLn . (' ':) . unwords . map show

readi :: Integral a =>  (ByteString -> Maybe (a, ByteString)) -> ByteString -> a
readi f s = let Just (n, _) = f s in n
