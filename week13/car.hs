module Car where
import Data.List.Split
import Data.List

printresults :: IO()
printresults = do
    x <- readFile "bids_201711.txt"
    let s = splitOn "\n" x
    let d = sortOn snd (createdata s)
    putStrLn("最高成交价："++ show (snd (last d)) ++"\n最低成交价："++ show (snd(head d)) ++"\n平均成交价："++ show (averageprice d)++"\n总共有"++show (length d)++"参与竞价"++ "\n成交名单：\n"++ (unlines (topten d 10)))
    writeFile "bidResults.txt" ("最高成交价："++ show (snd (last d)) ++"\n最低成交价："++ show (snd(head d)) ++"\n平均成交价："++ show (averageprice d)++"\n总共有"++show (length d)++"参与竞价"++ "\n成交名单：\n"++ (unlines (topten d 10)))

splitdata :: String -> [String]
splitdata x = splitOn "\n" x

createdata :: [String] -> [(Int,Int)]
createdata [] = []
createdata (x:xs) = [createdata2(x)]++createdata(xs)

createdata2 :: String -> (Int,Int)
createdata2 x = (read (head (words x)) :: Int,read (head (tail (words x))) :: Int)

sumprice :: [(Int,Int)] -> Int
sumprice [] = 0
sumprice (x:xs) = snd x + sumprice(xs)

averageprice:: [(Int,Int)] -> Float
averageprice x = (fromIntegral a)/ b
    where 
        a = fromIntegral (sumprice x)
        b = fromIntegral (length x)

topten::[(Int,Int)] -> Int -> [String]
topten _ 0 = []
topten x n =  [show (fst (last x)) ++"   " ++show(snd (last x))] ++ topten(init x)(n-1)