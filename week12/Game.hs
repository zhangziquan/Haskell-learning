-- 16340296 张子权 数据科学与计算机学院

module Game where
import System.Random

data Hand = Rock | Scissor | Paper deriving (Enum,Show,Eq)
instance Random Hand where
    random g = case randomR (0,2) g of
        (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
        (r, g') -> (toEnum r, g')

play:: IO ();
play = do
    handgame 0 0

randomHandIO :: IO Hand
randomHandIO = do
    number <- randomIO::IO Int
    let hand = number `mod` 3
    if hand == 0
        then
        return Rock
    else do
        if hand == 1
            then
            return Scissor
        else
            return Paper

userInput::String -> IO Hand
userInput input = do
    if input == "R" || input == "r"
        then
        return Rock
    else do 
        if input == "S" || input == "s"
            then
            return Scissor
        else do
            if input == "P" || input == "p"
                then
                return Paper
            else do
                return Rock

mycompare::Hand -> Hand -> IO Int
mycompare myhand yourhand = do
    if myhand == yourhand
        then
        return 0
    else do
        if myhand == Rock && yourhand == Scissor || myhand == Scissor && yourhand == Paper || myhand == Paper && yourhand == Rock
            then
            return 1
            else do
                return 2

handgame::Int -> Int -> IO()
handgame 3 _ = putStrLn("哈哈，我赢了！")
handgame _ 3 = putStrLn("算您赢了这轮。")
handgame mywin youwin = do
    putStr "请您出手 （R）石头,（S）剪刀，（P）布："
    myhand <- randomHandIO
    yourinput <- getChar
    n <- getChar
    yourhand <- userInput [yourinput]
    putStrLn("您出了"  ++ show yourhand ++ ", 我出了" ++ show myhand)
    
    f <- mycompare myhand yourhand
    let flag = f::Int
    if flag == 0 
        then do
            putStrLn("这一手算平手")
            putStrLn("我的得分：" ++ show mywin)
            putStrLn("您的得分：" ++ show youwin)
            handgame mywin youwin
        else if flag == 1
            then do
                putStrLn("我的得分：" ++ show (mywin+1))
                putStrLn("您的得分：" ++ show youwin)
                handgame (mywin+1) youwin
                else do
                    putStrLn("您赢了这手")
                    putStrLn("我的得分：" ++ show mywin)
                    putStrLn("您的得分：" ++ show (youwin+1))
                    handgame mywin (youwin+1)