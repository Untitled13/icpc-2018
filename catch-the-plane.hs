{-
:l /Users/cal/Desktop/cs/haskell/problems/catch-the-plane.hs
-}

import Data.Maybe

data Route = Route {start :: Int, destination ::Int, departure :: Int, arrival :: Int, odds::Double} deriving (Show,Eq)

solution :: Int -> Int -> Int -> [Route] -> Double
solution t tf station rs
     | t > tf           = 0
     | station == 1     = 1
     | rs == []         = 0
     | otherwise        = if temp == [] then 0 else maximum temp
          where nextRs  = filter ((>t) . departure)     rs
                ops     = filter ((station ==) . start) nextRs
                fn r    = odds r * solution (arrival r) tf (destination r) nextRs + (1 - odds r) * solution (departure r) tf station nextRs
                temp    = map fn ops

solve :: Int -> [Route] -> Double
solve tf rs = solution (-1) tf 0 rs

main :: IO ()
main = do
     line0 <- getLine
     line1 <- getLine
     let buses = (read $ (!!0) $ words line0) :: Int
     let tf    = (read line1) :: Int
     input <- sequence $ replicate buses getLine
     let mtrx  = map ((map (\x -> read x :: Double)) . words) input
     let rs    = map listToRoute mtrx
     print $ solve tf rs
     return ()


listToRoute :: [Double] -> Route
listToRoute [a,b,c,d,e] = Route (floor a) (floor b) (floor c) (floor d) e







