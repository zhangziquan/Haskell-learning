import System.Random
guess_number :: IO ()
guess_number = do
    some_number <- randomIO::IO Int
    let answer = some_number `mod` 101
    guess answer 0

guess :: Int-> Int -> IO ()
guess answer number = do
    putStrLn "guess between 0- 100"
    n <- getLine
    let m = read n :: Int
    if (m == answer)
        then putStrLn ("Good! You got it and the number of guesses is " ++ show (number+1))
        else
            if (m < answer)
                then
                    do putStrLn "too smaller"
                       guess answer (number+1)
                else
                    do putStrLn "too bigger"
                       guess answer (number+1)

data Hand = Rock | Scissor | Paper
    deriving (Show, Eq, Enum)
instance Random Hand where
    random g = case randomR (0,2) g of
            (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
            (r, g') -> (toEnum r, g')