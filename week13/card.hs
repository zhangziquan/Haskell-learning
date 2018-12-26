module Main where

main :: IO()
main = do
    num <- getLine
    let n = read num ::Integer
    getcard n


getcard::Integer -> IO()
getcard 0 = putStr("")
getcard num = do
    s <- getLine
    let card = read s ::Integer
    if(isValid card)
        then do
            putStrLn(s)
            getcard (num -1)
        else do
            getcard (num -1)

isValid :: Integer -> Bool
isValid x = if mod (mysum (doubleSec (addbit (dividecard x)))) 10 == 0 
    then True 
    else False

dividecard :: Integer -> [Integer]
dividecard 0 = []
dividecard cardnum = [mod (cardnum) 10] ++ dividecard (div cardnum 10)


addbit :: [Integer] ->[(Int,Integer)]
addbit s = zip [1..length s] s

doubleSec :: [(Int,Integer)] -> [Integer]
doubleSec [] = []
doubleSec (x:xs) = if(mod (fst x) 2 == 0)
    then [(snd x)*2] ++ doubleSec xs
    else [snd x] ++ doubleSec xs

mysum :: [Integer] -> Integer
mysum [] = 0
mysum (x:xs) = if(x<=9) 
    then x + mysum xs
    else mod (x) 10 + div x 10 + mysum xs