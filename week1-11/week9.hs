data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float deriving (Show, Eq)

perimeter :: Shape -> Float
perimeter (Circle r) = 3.14*2*r
perimeter (Rectangle l w) = 2*(l+w) 
perimeter (Triangle a b c) = a + b + c

isRound :: Shape -> Bool
isRound (Circle r) = True
-- isRound (Rectangle l w) = False
isRound _ = False

isRegular :: Shape -> Bool
isRegular (Circle r) = True
isRegular (Rectangle l w) = l==w
isRegular (Triangle a b c) = a==b&&b==c

area :: Shape -> Float
area (Circle r) = 3.14* r^2
area (Rectangle l w) = l*w
area (Triangle a b c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = (a+b+c)/2

data Item = String Float Float