module MyPicture where
import Data.Char
import Data.List

type Picture = [[Char]]

sideBySide :: Picture -> Picture -> Picture

sideBySide = zipWith (++)

sayit :: String -> IO ()
sayit = putStr.say

say :: String -> String
say str = unlines(printpic str (length(str)-1))

printpic :: String->Int->Picture
printpic str n
    |n == 1 = sideBySide(covert2pic (str!!0)) (covert2pic (str!!1))
    |otherwise = sideBySide(printpic str (n-1)) (covert2pic (str!!n))


covert2pic :: Char->Picture
covert2pic ch
    | ch == 'A' || ch =='a' = (lA)
    | ch == 'B' || ch =='b' = (lB)
    | ch == 'C' || ch =='c' = (lC)
    | ch == 'D' || ch =='d' = (lD)
    | ch == 'E' || ch =='e' = (lE)
    | ch == 'F' || ch =='f' = (lF)
    | ch == 'G' || ch =='g' = (lG)
    | ch == 'H' || ch =='h' = (lH)
    | ch == 'I' || ch =='i' = (lI)
    | ch == 'J' || ch =='j' = (lJ)
    | ch == 'K' || ch =='k' = (lK)
    | ch == 'L' || ch =='l' = (lL)
    | ch == 'M' || ch =='m' = (lM)
    | ch == 'N' || ch =='n' = (lN)
    | ch == 'O' || ch =='o' = (lO)
    | ch == 'P' || ch =='p' = (lP)
    | ch == 'Q' || ch =='q' = (lQ)
    | ch == 'R' || ch =='r' = (lR)
    | ch == 'S' || ch =='s' = (lS)
    | ch == 'T' || ch =='t' = (lT)
    | ch == 'U' || ch =='u' = (lU)
    | ch == 'V' || ch =='v' = (lV)
    | ch == 'W' || ch =='w' = (lW)
    | ch == 'X' || ch =='x' = (lX)
    | ch == 'Y' || ch =='y' = (lY)
    | ch == 'Z' || ch =='z' = (lZ)
    | ch == '0' = (l0)
    | ch == '1' = (l1)
    | ch == '2' = (l2)
    | ch == '3' = (l3)
    | ch == '4' = (l4)
    | ch == '5' = (l5)
    | ch == '6' = (l6)
    | ch == '7' = (l7)
    | ch == '8' = (l8)
    | ch == '9' = (l9)
    | otherwise = (lblank)

lA = ["  A    ",
    " A A   ",
    "A   A  ",
    "AAAAA  ",
    "A   A  "]
    
lB = ["BBBB   ",
    "B   B  ",
    "BBBB   ",
    "B   B  ",
    "BBBB   "]
    
lC = [" CCCC  ",
    "C      ",
    "C      ",
    "C      ",
    " CCCC  "]
    
lD = ["DDD    ",
    "D  D   ",
    "D   D  ",
    "D  D   ",
    "DDD    "]
    
lE = ["EEEEE  ",
    "E      ",
    "EEEEE  ",
    "E      ",
    "EEEEE  "]
    
lF = ["FFFFF  ",
    "F      ",
    "FFFF   ",
    "F      ",
    "F      "]
    
lG = [" GGGG  ",
    "G      ",
    "G  GG  ",
    "G   G  ",
    " GGG   "]
    
lH = ["H   H  ",
    "H   H  ",
    "HHHHH  ",
    "H   H  ",
    "H   H  "]
    
lI = ["IIIII  ",
    "  I    ",
    "  I    ",
    "  I    ",
    "IIIII  "]
    
lJ = [" JJJJ  ",
    "   J   ",
    "   J   ",
    "J  J   ",
    " JJ    "]
    
lK = ["K  KK  ",
    "K K    ",
    "KK     ",
    "K K    ",
    "K  KK  "]
    
lL = ["L      ",
    "L      ",
    "L      ",
    "L      ",
    "LLLLL  "]
    
lM = ["M   M  ",
    "MM MM  ",
    "M M M  ",
    "M M M  ",
    "M M M  "]
    
lN = ["N   N  ",
    "NN  N  ",
    "N N N  ",
    "N  NN  ",
    "N   N  "]
    
lO = [" OOO   ",
    "O   O  ",
    "O   O  ",
    "O   O  ",
    " OOO   "]
    
lP = ["PPPP   ",
    "P   P  ",
    "PPPP   ",
    "P      ",
    "P      "]
    
lQ = [" QQQ   ",
    "Q   Q  ",
    "Q   Q  ",
    "Q  Q   ",
    " QQ Q  "]
    
lR = ["RRRR   ",
    "R   R  ",
    "RR R   ",
    "R R    ",
    "R  RR  "]
    
lS = [" SSSS  ",
    "S      ",
    "SSSSS  ",
    "    S  ",
    "SSSS   "]
    
lT = ["TTTTT  ",
    "  T    ",
    "  T    ",
    "  T    ",
    "  T    "]
    
lU = ["U   U  ",
    "U   U  ",
    "U   U  ",
    "U   U  ",
    "UUUUU  "]
    
lV = ["V   V  ",
    "V   V  ",
    "V   V  ",
    " V V   ",
    "  V    "]
    
lW = ["W   W  ",
    "W W W  ",
    "W W W  ",
    "W W W  ",
    " W W   "]
    
lX = ["X   X  ",
    " X X   ",
    "  X    ",
    " X X   ",
    "X   X  "]
    
lY = ["Y   Y  ",
    " Y Y   ",
    "  Y    ",
    "  Y    ",
    "  Y    "]
    
lZ = ["ZZZZZ  ",
    "   Z   ",
    "  Z    ",
    " Z     ",
    "ZZZZZ  "]
    
l0 =[" 000   ",
    "00  0  ",
    "0 0 0  ",
    "0  00  ",
    " 000   "]
    
l1 =["  1    ",
    " 11    ",
    "  1    ",
    "  1    ",
    "11111  "]
    
l2 =[" 222   ",
    "2   2  ",
    "   2   ",
    "  2    ",
    "22222  "]
    
l3 =["3333   ",
    "    3  ",
    " 333   ",
    "    3  ",
    "3333   "]
    
l4 =["   4   ",
    "  44   ",
    " 4 4   ",
    "44444  ",
    "   4   "]
    
l5 =["55555  ",
    "5      ",
    "55555  ",
    "    5  ",
    "55555  "]
    
l6 =["66666  ",
    "6      ",
    "66666  ",
    "6   6  ",
    "66666  "]
    
l7 =["77777  ",
    "   7   ",
    "  7    ",
    "  7    ",
    "  7    "]
    
l8 =["88888  ",
    "8   8  ",
    "88888  ",
    "8   8  ",
    "88888  "]
    
l9 =["99999  ",
    "9   9  ",
    "99999  ",
    "    9  ",
    "99999  "]
    
lblank =["   ",
    "   ",
    "   ",
    "   ",
    "   "]