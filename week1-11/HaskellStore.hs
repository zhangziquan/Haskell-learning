module HaskellStore where
import Text.Printf
 -- 16340296 张子权

type Items = [Item]
type Item = (Name, Amount, Price)
-- name, amount and price per unit of the item
type Name = String -- name of the item
type Amount = Float -- amount, like kg or number
type Price = Float  -- price per unit


printItems :: Items -> IO ()
printItems item = do putStr(unlines (["Name     Amount       Price       Sum"] ++ convert item))


showPre :: Float -> String
showPre x = printf "%.2f" x


convert :: Items -> [String]
convert (x:items) = [a ++"     "++ showPre b++"     " ++ showPre c++"     "++ showPre d] ++ convert(items)
    where (a,b,c) = x
          d = b*c