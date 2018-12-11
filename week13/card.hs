isValid :: Integer -> Bool
isValid x = if mod (sum (splitNumGtTen (doubleSecondDigit (formARevList x)))) 10 == 0 then True else False

numValid :: [Integer] -> Integer
numValid xs = sum . map (\_ -> 1) $ filter isValid xs

formARevList :: Integer -> [Integer]
formARevList x
	|x <= 9 = [x]
	|otherwise = formARevList (mod x 10) ++ formARevList (div x 10)
	
doubleSecondDigit :: [Integer] -> [Integer]
doubleSecondDigit [x] = [x]
doubleSecondDigit (x:[y]) = [x] ++ [2*y]
doubleSecondDigit (x:y:xs) = [x] ++ [2*y] ++ doubleSecondDigit xs

splitNumGtTen :: [Integer] -> [Integer]
splitNumGtTen [x]
	|x <= 9 = [x]
	|otherwise = [div x 10] ++ [mod x 10]
splitNumGtTen (x:xs)
	|x <= 9 = [x] ++ splitNumGtTen xs
    |otherwise = [div x 10] ++ [mod x 10] ++ splitNumGtTen xs
