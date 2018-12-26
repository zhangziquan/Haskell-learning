module Newton_Raphson where
-- 张子权 16340296, ziquanzhang@126.com, 软件工程
-- 测试了求2的近似平方根以及其它函数。
squareroot2 :: Float -> Integer -> Float
squareroot2 x n =
    if n > 0 then squareroot2((x + 2/x)/2)(n-1) 
    else x

squareroot :: Float -> Float -> Integer -> Float
squareroot r x n = 
    if n > 0 then squareroot(r)((x + r/x)/2)(n-1)
    else x

sqrtSeq :: Float -> Float -> [Float]
sqrtSeq r x = [squareroot r x y | y <- [1..]]

squareroot' :: Float -> Float -> Float -> Float
squareroot' r x e =
    find (sqrtSeq(r)(x))(e)

find :: [Float] -> Float -> Float
find (x:xs) e =
    if(abs((head xs)-x) <=e) then x
    else find xs e