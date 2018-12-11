-- 张子权 16340296, 1417580438@qq.com, 软件工程
-- 测试了四则运算和乘法和除法对加法的分配律
module MyFraction where
import Test.QuickCheck

type Fraction = (Integer, Integer)

ratplus :: Fraction -> Fraction -> Fraction
ratplus (a,b) (c,d)
    | a == 0 = (c,d)
    | c == 0 = (a,b)
    | otherwise =(div (a*d+b*c) e, div (b*d) e)
    where
    e = gcd (a*d+b*c) (b*d)

ratminus :: Fraction -> Fraction -> Fraction
ratminus (a,b) (c,d) = (div (a*d-b*c) e, div (b*d) e)
    where
    e = gcd (a*d-b*c) (b*d)

rattimes :: Fraction -> Fraction -> Fraction
rattimes (a,b) (c,d) = (div (a*c) e, div (b*d) e)
    where
    e = gcd (a*c) (b*d)

ratdiv :: Fraction -> Fraction -> Fraction
ratdiv (a,b) (c,d) = (div (a*d) e, div (b*c) e)
    where
    e = gcd (a*d) (b*c)

ratfloor :: Fraction -> Integer
ratfloor (a,b) = div a b

ratfloat :: Fraction -> Float
ratfloat (a,b) = fromInteger a / fromInteger b

rateq :: Fraction -> Fraction -> Bool
rateq (a,b) (c,d) = 
    if ((a*d-b*c) == 0) then True else False

-- 定义新的运算符
infix 5 <+>
(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (a,b) (c,d) = ratplus (a,b) (c,d)
infix 5 <->
(<->) :: Fraction -> Fraction -> Fraction
(<->) (a,b) (c,d) = ratminus (a,b) (c,d)

infix 6 <-*->
(<-*->) :: Fraction -> Fraction -> Fraction
(<-*->) (a,b) (c,d) = rattimes (a,b) (c,d)

infix 6 </>
(</>) :: Fraction -> Fraction -> Fraction
(</>) (a,b) (c,d) = ratdiv (a,b) (c,d)

infix 4 <==>
(<==>) :: Fraction -> Fraction -> Bool
(<==>) (a,b) (c,d) = rateq (a,b) (c,d)


-- 测试加法
prop_ratplus_unit :: Fraction -> Property
prop_ratplus_unit (a,b) = b > 0 ==>(a, b) <+> (0,1) <==> (a, b)

-- 测试减法
prop_ratminus_unit :: Fraction -> Property
prop_ratminus_unit (a,b) = b > 0 ==>(a, b) <-> (0,1) <==> (a, b)

-- 测试乘法
prop_rattimes_unit :: Fraction -> Property
prop_rattimes_unit (a,b) = b > 0 ==>(a, b) <-*-> (0,1) <==> (0, 1)

-- 测试除法
prop_ratdiv_unit :: Fraction -> Property
prop_ratdiv_unit (a,b) = b > 0 && a> 0 ==>(a, b) </> (a,b) <==> (1, 1)

-- 测试乘法对加法可分配
prop_rattimes_plus_distr :: Fraction -> Fraction -> Fraction ->Property
prop_rattimes_plus_distr (a,b) (c,d) (e,f) = b > 0 && d > 0 && f > 0 ==> (a,b) <-*-> ((c,d) <+> (e,f)) <==> ((a,b) <-*-> (c,d)) <+> ((a,b) <-*-> (e,f))

-- 测试除法对加法可分配
prop_rattdiv_plus_distr :: Fraction -> Fraction -> Fraction ->Property
prop_rattdiv_plus_distr (a,b) (c,d) (e,f) = b > 0 && d > 0 && f > 0 && a > 0 && e > 0 && c > 0 ==> ((c,d) <+> (e,f))</>(a,b) <==> ((c,d) </> (a,b)) <+> ((e,f) </> (a,b))