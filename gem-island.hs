{-
:l /Users/cal/Desktop/cs/haskell/problems/icpc/worlds/2018/gem-island.hs
-}
import Data.Matrix
import Data.List
import Control.Applicative

--SOLVED! BUT SLOW

solve :: [Double] -> Int -> Int -> Int -> Double
solve xs pop 0    rich = sum $ take rich $ sortOn negate xs
solve xs pop days rich = sum $ prob
     where mtrx = fromList pop pop $ concat $ repeat xs
           next = toLists $ identity pop + mtrx
           vals = map (\ys -> solve ys pop (days - 1) rich) next
           prob = (*) <$> ZipList (map (/sum xs) xs) <*> ZipList vals

solution :: Int -> Int -> Int -> Double
solution pop days rich = solve (replicate pop 1) pop days rich

main :: IO ()
main = do 
     input <- getLine
     let xs = map (\x -> read x :: Int) $ words input
     print $ solution (xs !! 0) (xs !! 1) (xs !! 2)