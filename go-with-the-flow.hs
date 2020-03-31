import Data.List
import Data.Tuple
import Data.Function

--SOLVED!!!!

format :: String -> Int -> [String]
format "" _ = []
format s  n = let (_,v,r) = foldl (\(len,ans,rest) x -> 
                                   if len + (length x) > n 
                                   then (len+90,   ans,rest ++ x ++ " ")
                                   else (len+length x+1,ans ++ x ++ " ", rest)) (0,"","") $ words s in
                            (init v) : format r n

minus = (-)
sub = flip (-)

get :: [a] -> Int -> Maybe a
get []     _ = Nothing
get (x:xs) 0 = Just x
get (x:xs) n = get xs (n-1)

river :: Int -> [String] -> Int
river n []     = 0
river n [x]
     | x `get` n == Just ' ' = 1
     | otherwise             = 0
river n (x:xs)
     | x `get` n /= Just ' ' = 0
     | otherwise             = maximum $ map (1+) $ river <$> [n-1,n,n+1] <*> [xs]

flow :: [String] -> Int
flow []     = -1
flow (x:xs) = max (flow xs) (maximum $ river <$> [0..length x -1] <*> [x:xs])

longest :: Int -> (Int,Int) -> String -> (Int,Int)
longest width (max,mw) s
     | len `div` width <= max = (max,mw)
     | maxCurrent > max       = longest (width + 1) (maxCurrent, width) s
     | otherwise              = longest (width + 1) (max,        mw)    s
     where len                = length s
           maxCurrent         = flow $ format s width

solution :: String -> (Int,Int)
solution s = let min = maximum $ length <$> words s
             in  swap $ longest min (-1,0) s

main :: IO ()
main = do
     n <- getLine
     str <- getLine
     let sol = solution str
     putStrLn $ (show $ fst sol) ++ " " ++ (show $ snd sol)
