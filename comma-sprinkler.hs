
import Data.Char
import Data.Function
import Data.List

onBoth f g h x = f (g x) (h x)

minus = (-)

-- SOLVED

solution :: String -> String
solution s         = if result == s then result else solution result
     where divided = words s
           afters  = map (filter isLetter) $ filter (',' `elem`) $ divided
           before  = map (filter isLetter . snd) $ filter (elem ',' . fst) $ zip divided $ tail divided
           word    = map (filter (onBoth (||) (=='.') isLetter)) divided
           fn as w = if w `elem` as then w ++ "," else w
           nopunc  = map (filter isLetter) word
           befores = (map fst $ filter ((flip elem) before . snd) $ zip nopunc $ tail nopunc)
           result  = concat $ intersperse " " $ map (fn afters . fn befores) word

main :: IO ()
main = do 
     input <- getLine
     putStrLn $ solution input



