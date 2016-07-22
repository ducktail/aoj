import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main :: IO ()
main = do
  di <- getl toInt
  unless (di == 0) $ do
    [dh,dm] <- getl $ wrds toInt
    ai <- getl toInt
    [ah,am] <- getl $ wrds toInt
    print $ solve di (dh * 100 + dm) ai (ah * 100 + am)
    main

solve :: Int -> Int -> Int -> Int -> Int
solve di dt ai at
  | f dtbl (di,ai) <= 40 && ((at >= 1730 && at <= 1930) || (dt >= 1730 && dt <= 1930)) = g (f ttbl (di,ai))
                                                                                                                   | otherwise = f ttbl (di,ai)
  where
    g x = let y = x `div` 50 in if even y then y `div` 2 * 50 else y `div` 2 * 50 + 50
    f t x = maybe 0 id $ lookup x t
    ttbl = [((1,2),300),((1,3),500),((1,4),600),((1,5),700),((1,6),1350),((1,7),1650),
                        ((2,3),350),((2,4),450),((2,5),600),((2,6),1150),((2,7),1500),
                                    ((3,4),250),((3,5),400),((3,6),1000),((3,7),1350),
                                                ((4,5),250),((4,6),850), ((4,7),1300),
                                                            ((5,6),600), ((5,7),1150),
                                                                         ((6,7),500)]
    dtbl = [((1,2),6),((1,3),13),((1,4),18),((1,5),23),((1,6),43),((1,7),58),
                      ((2,3),7), ((2,4),12),((2,5),17),((2,6),37),((2,7),52),
                                 ((3,4),5), ((3,5),10),((3,6),30),((3,7),45),
                                            ((4,5),5), ((4,6),25),((4,7),40),
                                                       ((5,6),20),((5,7),35),
                                                                  ((6,7),15)]

toInt :: String -> Int
toInt s = read s

wrds :: (String -> a) -> String -> [a]
wrds f = map f . words

getl :: (String -> a) -> IO a
getl f = f <$> getLine
