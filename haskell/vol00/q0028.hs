import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.List

main = do
  xs <- input read :: IO [Int]
  mapM_ print $ solve xs
 
solve :: [Int] -> [Int]
solve = fmt . anf
 
anf :: [Int] -> [(Int,Int)]
anf = reverse . sort . map (\x -> (length x, head x)) . group . sort
 
fmt :: [(Int,Int)] -> [Int]
fmt [x] = [snd x]
fmt (x:xs) = sort $ map snd (x : filter (\y -> fst x == fst y) xs)
 
input :: (String -> a) -> IO [a]
input f = map f . lines <$> getContents
