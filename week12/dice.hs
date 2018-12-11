-- 16340296 张子权 数据科学与计算机学院
module Dice where
import System.Random

flipDice :: IO Bool
flipDice = do
    number <- randomIO::IO Int
    let dice = number `mod` 6
    if dice == 5
        then
        return True
    else do
        return False

flipManyDices ::Int -> Int -> IO Bool
flipManyDices _ 0 = return True
flipManyDices n m = do
    if n > 0 
        then do
        t <- flipDice
        if t == True
            then
            flipManyDices (n-1) (m-1)
        else do
            flipManyDices (n-1) m
    else do
        return False


flipManytimes :: Int-> Int-> Int-> IO Float
flipManytimes n num m = do 
    xs <- getDices n num m
    let y = countIt xs
    return (fromIntegral y / fromIntegral n)


countIt :: [Bool] -> Int
countIt xs = length [x|x<- xs , x == True]

getDices :: Int-> Int -> Int-> IO [Bool]
getDices 0 _ _ = return []
getDices n num m= do
    r <- flipManyDices num m
    rs <- getDices (n-1) num m
    return (r:rs)