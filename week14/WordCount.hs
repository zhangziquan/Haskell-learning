module Main where
import Data.List
import Data.Char

mytoLower :: Char -> Char
mytoLower x =
    if isAlpha x || x == '-' || x == '\'' || isNumber x
        then toLower x
    else
        ' '

tolow :: String -> String
tolow x = [mytoLower a | a <- x ]

getWord :: [Char] -> [Char]
getWord ws = takeWhile (\x -> isAlpha x  || x =='-' || isNumber x ) ws

step0::String -> [String]
step0 s = words (tolow s)

step1::[String] -> [String]
step1 s = [ getWord x | x <- s ]

step2::[String] -> [String]
step2 s = sort s

step3::[String] -> [[String]]
step3 s = group s

step4::[[String]]->[(String,Int)]
step4 xs = [(head x , length x) | x <-xs ]



main ::IO()
main = do
   ls <- readFile "text.txt"
   let result = string2listofpairs ls
   let formatedResult = formatting result
   writeFile "answer.txt" formatedResult

string2listofpairs :: String ->[(String, Int)] 
string2listofpairs s = bubbleSort(step4 (step3 (step2 (step1 (step0 s)))))

formatting :: [(String, Int)] ->String
formatting (x:xs) =
        if fst x == ""
            then "" ++ formatting xs
        else 
            fst x ++ " " ++ show (snd x)++"\n" ++ formatting xs
formatting [] = ""  


swaps ::[(String,Int)] -> [(String,Int)]
swaps [] = []
swaps [(x,y)] = [(x,y)]
swaps ((x1,y1):(x2,y2):xs)
        | y2 > y1 = (x2,y2) : swaps((x1,y1):xs)
        | y2 == y1 && x2 < x1 = (x2,y2) : swaps((x1,y1):xs)
        | otherwise = (x1,y1) : swaps((x2,y2):xs)

bubbleSort ::[(String,Int)] -> [(String,Int)]
bubbleSort xs
        | swaps xs == xs = xs       -- 没发生变化，就停止
        | otherwise = bubbleSort $ swaps xs