{-
/Users/cal/Desktop/cs/haskell/problems/icpc/worlds/2018/conquer-the-world.hs
-}

import Data.List
import Data.Function
import Data.Maybe

--SOLVED!!

data Node = Node {paths :: [Path], current :: Int, goal :: Int} deriving (Show, Eq)

data Path = Path {dest :: Vertex, price :: Int} deriving (Show, Eq)

type Map = [Node]

type Vertex = Int 

onBoth :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
onBoth f g h x = f (g x) (h x)
 
ops :: Map -> Vertex -> Node
ops m v = m !! v

relu :: (Ord a, Num a) => a -> a
relu x
     | x <= 0    = 0
     | otherwise = x

applyIndex :: (a -> a) -> Int -> [a] -> [a]
applyIndex f 0 (x:xs) = f x : xs
applyIndex f n (x:xs) = x : applyIndex f (n-1) xs

chngCurrent :: Int -> Node -> Node
chngCurrent n (Node ps c g) = Node ps (c+n) g

move :: Map -> Vertex -> Vertex -> Int -> Map
move m origin destin n = applyIndex (chngCurrent (n)) destin . applyIndex (chngCurrent (-n)) origin $ m

solve :: Map -> Int
solve m = fromJust $ head $ dropWhile isNothing $ map (solution m) [0..]

solution :: Map -> Int -> Maybe Int
solution m bound
     | bound < 0                        = Nothing
     | all (onBoth (>=) current goal) m = Just 0
     | rest < 0                         = (>>= Just . minimum) $ (\x -> if x == [] then Nothing else Just x) $ map fromJust $ filter isJust options
     | otherwise                        = (>>= Just . minimum) $ (\x -> if x == [] then Nothing else Just x)   nexts
     where sorted   = sortOn (negate . onBoth (-) current goal) m
           rest     = sum $ map (onBoth (-) current goal) $ tail sorted
           index    = fromJust $ elemIndex (head sorted) m
           options  = map (\(Path d p) -> (+) <$> Just p <*> solution (move m index d 1) (bound - p)) $ paths $ ops m index
           possible = takeWhile ((>=0) . onBoth (-) current goal) m
           indices  = map (fromJust . (`elemIndex` m)) possible
           nexts    = map fromJust $ filter isJust $ concat $ map (\(n,i) -> map (\path -> (+) <$> Just (price path) <*> solution (move m i (dest path) 1) (bound - price path)) $ paths n) $ zip possible indices

minus = flip (-)

main :: IO ()
main = do
     n_ <- getLine
     let n = read n_ :: Int
     ps <- sequence (replicate (n-1) getLine)
     let paths = map (\[u,v,c] -> (u-1, Path (v - 1) c)) $ map (map read) $ map words $ ps
     let duplicates = paths >>= (\(u, Path v c) -> [(u, Path v c), (v, Path u c)])
     ns <- sequence (replicate n getLine)
     let better = map (map snd) $ groupBy ((==) `on` fst) $ sortOn fst duplicates
     let nations = map (\[c,g] p -> Node p c g) $ map (map read) $ map words ns
     let graph   = zipWith (\f x -> f x) nations better
     print $ solve graph

example1 = [Node [Path 1 5, Path 2 5] 2 1, Node [Path 0 5] 5 0, Node [Path 0 5] 1 3]





