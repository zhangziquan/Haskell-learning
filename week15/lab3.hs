module Lab3 where

import Data.List
data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Or Prop Prop | Imply Prop Prop deriving Eq

p1 = And (Var 'A') (Not (Var 'A'))
p2 = Or (Var 'A') (Not (Var 'A'))
p3 = Imply (Var 'A')(And (Var 'A') (Var 'B'))

a :: [Prop]
a = [
    Const False]

instance Show Prop where
    show (Const True) = "True"
    show (Const False) = "False"
    show (Var p) = show p
    show (Not p) = "~" ++ show p
    show (And p q) = (show p) ++ "&&" ++ (show q)
    show (Or p q) = (show p) ++ "||" ++ (show q)
    show (Imply p q) = (show p) ++ "=>" ++ (show q)

type Subst = [(Char,Bool)]

eval:: Subst -> Prop -> Bool
eval sub (Const p) = p
eval sub (Var p) = x
    where x = head [b|(a,b)<-sub,p == a]
eval sub (Not p) = not (eval sub p)
eval sub (And p q) = (eval sub p) && (eval sub q)
eval sub (Or p q) = (eval sub p) || (eval sub q)
eval sub (Imply p q) = (not (eval sub p))||((eval sub p) && (eval sub q))


vars:: Prop -> [Char]
vars (Const p) = ['p']
vars (Var p) = [p]
vars (Not p) = vars p
vars (And p q) = nub((vars p) ++ (vars q))
vars (Or p q) = nub((vars p) ++ (vars q))
vars (Imply p q) = nub((vars p) ++ (vars q))

substs :: Prop->[Subst]
substs p = getsub (vars p)

getsub::[Char] -> [Subst]
getsub [] = []
getsub [a] = [[(a,True)]] ++ [[(a,False)]]
getsub (x:xs) = [(x,True):a|a<-getsub(xs)] ++ [(x,False):a|a<-getsub(xs)]

isTaut::Prop->Bool
isTaut p = isTauthelper p (substs p)

isTauthelper:: Prop -> [Subst] -> Bool
isTauthelper p [] = True
isTauthelper p [x] = (eval x p)
isTauthelper p (x:xs) = 
    if(eval x p)
        then isTauthelper p xs
    else
        False